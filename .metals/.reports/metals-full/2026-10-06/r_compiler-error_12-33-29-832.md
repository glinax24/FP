error id: 0157B5BB0ABFBFB78F114A55A5088FAE
file:///D:/FP/hello-world/src/main/scala/transfer/operations/Writer.scala
### java.lang.NullPointerException: Cannot invoke "dotty.tools.dotc.parsing.Scanners$Region.commasExpectedInEnclosing()" because the return value of "dotty.tools.dotc.parsing.Scanners$Indented.outer()" is null

occurred in the presentation compiler.



action parameters:
offset: 4694
uri: file:///D:/FP/hello-world/src/main/scala/transfer/operations/Writer.scala
text:
```scala
package transfer.operations

import transfer.config.Config
import transfer.domain.*
import transfer.planning.{Plan, Planner}
import transfer.util.{Checksum, Fs, Logger, SizeFormat}

import java.io.{BufferedOutputStream, InputStream, OutputStream}
import java.nio.channels.{Channels, FileChannel}
import java.nio.file.{Files, Path, StandardOpenOption}
import java.security.MessageDigest
import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal

//Writer выполняет физическую запись запланированных частей файлов на носители и 
//поддерживает продолжение прерванной записи

//хранит текущее состояние и прогресс записи  
case class WriteProgress(
    setId: String,
    completedMedia: Int,
    totalMedia: Int,
    // Номера частей, запись которых завершена и которые прошли проверку
    verifiedParts: List[Int],
    lastVerifiedPart: Option[Int],
    nextPart: Option[Int],
    manifest: Manifest,
    //Где сохранён манифест набора  
    manifestPath: String = "",
    // Итоговый манифест лёг на первый носитель; иначе он остался во временном каталоге  
    manifestOnFirstMedia: Boolean = true
)

//
// Запись частей на носители и продолжение прерванной записи.
// Части записываются последовательно, носитель заполняется целиком и только после
// успешной проверки считается готовым. Носители не обязаны быть подключены одновременно:
// перед каждым носителем программа запрашивает его у пользователя, а прогресс сохраняется
// в рабочем каталоге.
object Writer:

  private val BufferSize = 64 * 1024

  //хранит список носителей, их количество и конфигурацию
  private case class Ctx(media: List[Media], totalMedia: Int, config: Config):
    // обновляет путь/информацию о конкретном носителе в Ctx
    def reconnected(number: Int, media: Media): Ctx = copy(media = this.media.updated(number - 1, media))

  //накапливает результаты записи: проверенные части, контрольные суммы и завершённые носители
  private case class Acc(
      verifiedParts: List[Int],
      partChecksums: Map[Int, String],
      digests: DigestBook,
      completedMedia: Int
  )

  //
  // Контрольные суммы исходных файлов, считаемые по ходу записи: данные не перечитываются
   // повторно. Файлы, чьи контрольные суммы уже известны из манифеста, не пересчитываются.


  //считает и хранит контрольные суммы исходных файлов во время записи
  private class DigestBook(val algorithm: String, files: List[SourceEntry.File], known: Map[String, String]):
    private val digests = mutable.Map.from(files.map(f => f.relPath -> MessageDigest.getInstance(algorithm)))
    private val left = mutable.Map.from(files.map(f => f.relPath -> f.size))
    private val done = mutable.Map.from(known.filter((k, _) => files.exists(_.relPath == k)))

    //принимает очередную порцию байтов файла, добавляет её в расчёт SHA-256 и отслеживает, 
    //сколько байтов ещё осталось. Когда файл полностью прочитан — сохраняет его контрольную сумму
    def consume(relPath: String, buffer: Array[Byte], count: Int): Unit =
      if !done.contains(relPath) then
        digests(relPath).update(buffer, 0, count)
        val rest = left(relPath) - count
        if rest <= 0 then done.update(relPath, Checksum.finish(digests(relPath)))
        else left.update(relPath, rest)

    //возвращает список файлов, для которых контрольная сумма уже полностью посчитана
    def completed: Map[String, String] = done.toMap
    //возвращает список файлов, для которых контрольная сумма ещё не посчитана
    def missing: List[SourceEntry.File] = files.filterNot(f => done.contains(f.relPath))

  //Записывает все части плана, по очереди заполняя носители и проверяя каждую часть
  def writeSet(
      set: TransferSet,
      plan: Plan,
      media: List[Media],
      config: Config,
      request: Option[MediaRequest] = None
  ): Either[AppError, WriteProgress] =
    start(set, plan, media, config, Nil, Map.empty, Map.empty, 0, request)

  //
  // Продолжает прерванную запись: уже проверенные части не перезаписываются,
  // незавершённая часть создаётся заново.
  def continue(
      set: TransferSet,
      plan: Plan,
      progress: WriteProgress,
      media: List[Media],
      config: Config,
      request: Option[MediaRequest] = None
  ): Either[AppError, WriteProgress] =
    start(
      set,
      plan,
      media,
      config,
      progress.verifiedParts,
      progress.manifest.parts.flatMap(p => p.checksum.map(p.number -> _)).toMap,
      progress.manifest.files.map(f => f.relPath -> f.checksum).filterNot((_, c) => c.isEmpty).toMap,
      progress.completedMedia,
      request
    )

  //
   // Возобновляет прерванную запись по состоянию, сохранённому в рабочем каталоге.
   @@* Используется после перезапуска программы: заново создаётся только та часть,
   * которая не была проверена, уже проверенные части не трогаются.
   */
  def resume(
      set: TransferSet,
      plan: Plan,
      media: List[Media],
      config: Config,
      request: Option[MediaRequest] = None
  ): Either[AppError, WriteProgress] =
    val workDir = Fs.toPath(config.workDirectory)
    for
      session  <- SessionIO.load(workDir).flatMap {
        case None              => Left(AppError.MissingState("нет сохранённого состояния записи — выполните write"))
        case Some(s) if s.setId != set.id => Left(AppError.MissingState(s"сохранённое состояние относится к набору ${s.setName} (${s.setId})"))
        case Some(s)           => Right(s)
      }
      manifest <- ManifestIO.readManifest(workDir.resolve(config.manifestFile))
      _        <- if manifest.setId != set.id then
        Left(AppError.MissingState(s"манифест в рабочем каталоге относится к другому набору: ${manifest.name} (${manifest.setId})"))
      else Right(())
      _        <- checkSources(set)
      progress  = WriteProgress(
        set.id,
        session.completedMedia,
        session.totalMedia,
        session.verifiedParts,
        session.verifiedParts.lastOption,
        session.nextPart,
        manifest
      )
      _         = Logger.info(
        s"Продолжение записи набора «${set.name}»: носителей завершено ${session.completedMedia} из ${session.totalMedia}, проверено частей ${session.verifiedParts.size}.",
        config.logFile
      )
      result   <- continue(set, plan, progress, media, config, request)
    yield result

  //подготавливает данные и запускает процесс записи
  private def start(
      set: TransferSet,
      plan: Plan,
      media: List[Media],
      config: Config,
      verifiedParts: List[Int],
      partChecksums: Map[Int, String],
      fileChecksums: Map[String, String],
      completedMedia: Int,
      request: Option[MediaRequest]
  ): Either[AppError, WriteProgress] =
    val ctx = Ctx(media, media.size, config)
    val acc = Acc(verifiedParts.sorted, partChecksums, new DigestBook(config.checksumAlgorithm, filesOf(set), fileChecksums), completedMedia)
    Logger.info(
      s"Начало записи набора «${set.name}»: частей ${plan.parts.size}, носителей ${media.size}",
      config.logFile
    )
    for
      _        <- checkSources(set)
      contents <- PartPacker.pack(filesOf(set), plan.parts)
      // Рабочая копия манифеста всегда доступна в каталоге приложения — она нужна для продолжения записи.
      _        <- storeManifest(buildManifest(set, plan, media, config, acc, completed = false), ctx, finalCopy = false)
      result   <- run(set, plan, contents, ctx, completedMedia, acc, request, attempts = 0)
    yield result

  /**
   * последовательно записывает части на каждый носитель: носитель запрашивается у пользователя,
   * его реальный путь и объём перечитываются, уже записанные носители не изменяются.
   */
  private def run(
      set: TransferSet,
      plan: Plan,
      contents: List[PartContent],
      ctx: Ctx,
      from: Int,
      acc: Acc,
      request: Option[MediaRequest],
      attempts: Int
  ): Either[AppError, WriteProgress] =
    if from >= ctx.media.size then complete(set, plan, ctx, acc)
    else
      val number = from + 1
      connectMedia(ctx, number, request, attempts) match
        case Left(err) => Left(err)
        case Right((nextCtx, current)) if current.path != ctx.media(from).path =>
          // Носитель подключён по другому пути: план и манифест должны знать фактический.
          run(set, plan, contents, nextCtx, from, acc, request, attempts + 1)
        case Right((nextCtx, current)) =>
          val root = Fs.toPath(current.path)
          val pending = plan.parts.filter(p => p.mediaNumber == number && !acc.verifiedParts.contains(p.number))
          val required = pending.map(_.size).sum
          Logger.info(
            s"Носитель $number из ${ctx.totalMedia} (${current.label}): начало записи, частей ${pending.size}, " +
            s"требуется ${SizeFormat.humanReadable(required)}",
            ctx.config.logFile
          )
          for
            _        <- checkMedia(root, number, set.id, ctx.config)
            (p2, c2) <- ensureSpace(set, plan, contents, ctx, from, current, root, pending, required, acc)
            myParts   = p2.parts.filter(_.mediaNumber == number)
            acc2     <- writePartsOnMedia(set, c2, myParts, root, ctx, acc)
            acc3     <- completeMedia(set, p2, root, current, ctx, number, myParts, acc2)
            acc4     <- run(set, p2, c2, ctx, from + 1, acc3, request, 0)
          yield acc4

  //одключает очередной носитель и проверяет его путь
  private def connectMedia(
      ctx: Ctx,
      number: Int,
      request: Option[MediaRequest],
      attempts: Int
  ): Either[AppError, (Ctx, Media)] =
    val planned = ctx.media(number - 1)
    val actual = request match
      case None => Right(planned.path)
      case Some(ask) => ask.path(number, ctx.totalMedia, planned)
    actual.flatMap { path =>
      if attempts >= 5 then Left(AppError.MediaUnavailable(s"носитель $number (${planned.label}): путь не определён после нескольких попыток"))
      else if !Fs.isDirectory(path) then
        Left(
          AppError.MediaUnavailable(
            s"носитель $number из ${ctx.totalMedia} (${planned.label}): каталог $path недоступен.\n" +
            "Подключите носитель и повторите запись: выполните continue."
          )
        )
      else Right((ctx.reconnected(number, Media.connectedTo(planned, path)), Media.connectedTo(planned, path)))
    }

  //Признаёт носитель готовым: пишет описание носителя, обновляет манифест и сообщает о готовности
  private def completeMedia(
      set: TransferSet,
      plan: Plan,
      root: Path,
      current: Media,
      ctx: Ctx,
      number: Int,
      myParts: List[Part],
      acc: Acc
  ): Either[AppError, Acc] =
    val done = acc.copy(completedMedia = number)
    writeDescriptor(root, set, number, ctx, myParts.map(_.number))
      .flatMap(_ => storeManifest(buildManifest(set, plan, ctx.media, ctx.config, done, completed = false), ctx, finalCopy = false))
      .map { _ =>
        val message = mediaReadyMessage(current, number, ctx, myParts.size)
        Logger.info(message.replace("\n\n", ". ").replace('\n', ' '), ctx.config.logFile)
        done
      }

  /**
   * Проверяет, что на носителе хватает места для оставшихся частей.
   * Если доступного объёма стало меньше, чем при планировании, оставшаяся часть
   * плана перестраивается; если перестроить невозможно — запись приостанавливается.
   */
  private def ensureSpace(
      set: TransferSet,
      plan: Plan,
      contents: List[PartContent],
      ctx: Ctx,
      from: Int,
      current: Media,
      root: Path,
      pending: List[Part],
      required: Long,
      acc: Acc
  ): Either[AppError, (Plan, List[PartContent])] =
    if required <= 0 then Right(plan -> contents)
    else
      val usable = Media.usableSpace(current.copy(availableSpace = Fs.availableSpace(root)), ctx.config.reservedSpacePerMedia)
      if usable >= required then Right(plan -> contents)
      else
        Planner.replanTail(set, plan, acc.verifiedParts, ctx.media.drop(from), ctx.config) match
          case Right(p2) =>
            Logger.info(
              s"Носитель ${current.label}: доступного места меньше планового (${SizeFormat.humanReadable(usable)}), остаток плана перестроен",
              ctx.config.logFile
            )
            val tailParts = p2.parts.filterNot(p => acc.verifiedParts.contains(p.number))
            val consumed = consumedBytes(plan, acc.verifiedParts)
            PartPacker.packFrom(filesOf(set), tailParts, consumed).map(c2 => p2 -> c2)
          case Left(err) =>
            saveSession(set, ctx, acc)
            Logger.warn(
              s"Недостаточно места на носителе ${current.label}: требуется ${SizeFormat.humanReadable(required)}, доступно ${SizeFormat.humanReadable(usable)}",
              ctx.config.logFile
            )
            Left(
              AppError.Suspended(
                s"Носитель: ${current.label}\n" +
                s"Часть: ${pending.headOption.map(p => s"${p.number} из ${plan.parts.size}").getOrElse("-")}\n" +
                s"Причина: недостаточно свободного места, не хватает ${SizeFormat.humanReadable(required - usable)}.\n\n" +
                "Успешно записанные части сохранены.\n" +
                "Подключите дополнительный носитель, освободите место или измените порядок носителей; затем выполните continue."
              )
            )
  //считает, сколько байт уже записано
  private def consumedBytes(plan: Plan, verified: List[Int]): Long =
    plan.parts.filter(p => verified.contains(p.number)).map(_.size).sum

  //записывает все части, предназначенные для текущего носителя
  private def writePartsOnMedia(
      set: TransferSet,
      contents: List[PartContent],
      parts: List[Part],
      root: Path,
      ctx: Ctx,
      acc: Acc
  ): Either[AppError, Acc] =
    val byNumber = contents.map(c => c.number -> c).toMap
    parts match
      case Nil => Right(acc)
      case part :: tail =>
        val file = root.resolve(part.fileName)
        // Уже проверенная часть не перезаписывается, но если её файла нет — часть создаётся заново.
        if acc.verifiedParts.contains(part.number) && Files.exists(file) then
          Logger.info(s"Носитель ${part.mediaNumber}: часть ${part.number} уже проверена, пропускается", ctx.config.logFile)
          writePartsOnMedia(set, contents, tail, root, ctx, acc)
        else
          byNumber.get(part.number) match
            case None => Left(AppError.PartMissing(part.number))
            case Some(content) =>
              val written =
                writePartFile(content, root, acc.digests).flatMap { _ =>
                  // Проверка сразу после записи (§5): файл создан, размер совпадает, запись завершена.
                  if ctx.config.verifyAfterWrite then Verifier.verifyPart(part, root, ctx.config)
                  else Verifier.verifyPartWritten(part, root, ctx.config)
                }
              written
                .flatMap { actual =>
                  // Пересозданная часть уже числится проверенной — номер не должен дублироваться.
                  val verified =
                    if acc.verifiedParts.contains(part.number) then acc.verifiedParts
                    else (acc.verifiedParts :+ part.number).sorted
                  val acc2 = acc.copy(verifiedParts = verified, partChecksums = acc.partChecksums.updated(part.number, actual))
                  Logger.info(
                    s"Носитель ${part.mediaNumber}: часть ${part.number} создана и проверена (${SizeFormat.humanReadable(part.size)})",
                    ctx.config.logFile
                  )
                  saveSession(set, ctx, acc2).map(_ => acc2)
                }
                .flatMap(acc2 => writePartsOnMedia(set, contents, tail, root, ctx, acc2))

  /** Пишет данные части в файл, одновременно считая контрольные суммы исходных файлов. */
  private def writePartFile(content: PartContent, root: Path, digests: DigestBook): Either[AppError, Unit] =
    try
      Files.createDirectories(root)
      val out = new BufferedOutputStream(
        Files.newOutputStream(root.resolve(content.fileName), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING, StandardOpenOption.WRITE),
        BufferSize
      )
      try writeSlices(content.slices, out, digests)
      finally out.close()
    catch
      case NonFatal(e) => Left(AppError.MediaNotWritable(s"$root (не удалось записать часть ${content.number}: ${e.getMessage})"))

  //последовательно записывает фрагменты исходных файлов в одну часть
  private def writeSlices(slices: List[FileSlice], out: OutputStream, digests: DigestBook): Either[AppError, Unit] =
    val buffer = new Array[Byte](BufferSize)
    slices match
      case Nil          => Right(())
      case slice :: tail =>
        copySlice(slice, out, buffer, digests).flatMap(_ => writeSlices(tail, out, digests))

  //читает конкретный фрагмент исходного файла
  private def copySlice(
      slice: FileSlice,
      out: OutputStream,
      buffer: Array[Byte],
      digests: DigestBook
  ): Either[AppError, Unit] =
    val path = Fs.toPath(slice.sourcePath)
    if !Files.isReadable(path) then Left(AppError.SourceUnreadable(slice.sourcePath))
    else
      try
        val channel = FileChannel.open(path, StandardOpenOption.READ)
        val in = Channels.newInputStream(channel)
        try
          channel.position(slice.offset)
          copyRest(in, out, slice, buffer, digests, slice.length)
        finally in.close()
      catch case NonFatal(e) => Left(AppError.SourceUnreadable(s"${slice.relPath} (${e.getMessage})"))

  @tailrec //Эта функция должна использовать хвостовую рекурсию, проверь это».
          //Хвостовая рекурсия — когда функция вызывает саму себя последним действием
  
  //копирует данные фрагмента небольшими блоками и считает контрольную сумму
  private def copyRest(
      in: InputStream,
      out: OutputStream,
      slice: FileSlice,
      buffer: Array[Byte],
      digests: DigestBook,
      left: Long
  ): Either[AppError, Unit] =
    if left <= 0 then Right(())
    else
      val read = in.read(buffer, 0, Math.min(buffer.length.toLong, left).toInt)
      if read <= 0 then Left(AppError.SizeMismatch(s"фрагмент ${slice.relPath}", slice.length, slice.length - left))
      else
        out.write(buffer, 0, read)
        digests.consume(slice.relPath, buffer, read)
        copyRest(in, out, slice, buffer, digests, left - read)

  /** Проверяет, что носитель доступен, защищён от записи и принадлежит этому набору  */
  private def checkMedia(root: Path, number: Int, setId: String, config: Config): Either[AppError, Unit] =
    if !Files.isDirectory(root) then Left(AppError.MediaUnavailable(root.toString))
    else if !Files.isWritable(root) then Left(AppError.MediaNotWritable(root.toString))
    else
      // Служебные файлы, оставшиеся от чужого набора, блокируют запись (§14).
      foreignManifestFailure(root, setId, config) match
        case Some(err) => Left(err)
        case None =>
          ManifestIO.readDescriptor(root, config.mediaDescriptorFile).flatMap {
            case None => Right(())
            case Some(d) if d.setId != setId => Left(AppError.WrongMedia(s"носитель $number набора $setId", s"носитель набора ${d.setId} (${d.name})"))
            case Some(d) =>
              if d.mediaNumber != number then Left(AppError.MediaWrongNumber(number, d.mediaNumber))
              else
                Logger.warn(s"Носитель $number уже содержит части набора ${d.name}: ненайденные части будут перезаписаны", config.logFile)
                Right(())
          }

  //проверяет, нет ли на носителе манифеста другого набора
  private def foreignManifestFailure(root: Path, setId: String, config: Config): Option[AppError] =
    val file = root.resolve(config.manifestFile)
    if !Files.exists(file) then None
    else
      ManifestIO.readManifest(file) match
        case Left(err)                         => Some(err)
        case Right(m) if m.setId != setId      => Some(AppError.WrongMedia(s"носитель набора $setId", s"носитель набора ${m.setId} (${m.name})"))
        case Right(_)                          => None

  //записывает описание носителя (MediaDescriptor)
  private def writeDescriptor(root: Path, set: TransferSet, number: Int, ctx: Ctx, partNumbers: List[Int]): Either[AppError, Unit] =
    ManifestIO.writeDescriptor(root, MediaDescriptor(set.id, set.name, number, ctx.totalMedia, partNumbers, completed = true), ctx.config.mediaDescriptorFile)

  // Манифест хранится на первом носителе и в рабочем каталоге 
  private def storeManifest(manifest: Manifest, ctx: Ctx, finalCopy: Boolean): Either[AppError, Boolean] =
    val workCopy = ManifestIO.writeManifest(manifest, Fs.toPath(ctx.config.workDirectory), ctx.config.manifestFile)
    val onFirstMedia = ctx.media.headOption match
      case None => Left(AppError.MediaUnavailable("первый носитель не добавлен"))
      case Some(m) =>
        val root = Fs.toPath(m.path)
        if Files.isDirectory(root) && Files.isWritable(root) then ManifestIO.writeManifest(manifest, root, ctx.config.manifestFile)
        else Left(AppError.MediaUnavailable(m.path))
    val stored = onFirstMedia match
      case Right(()) =>
        Logger.info(
          s"Манифест набора «${manifest.name}» обновлён на первом носителе (${ctx.media.head.path}\\${ctx.config.manifestFile}), " +
          s"завершён: ${if manifest.completed then "да" else "нет"}",
          ctx.config.logFile
        )
        Right(true)
      case Left(err) if finalCopy =>
        // Первый носитель отключён на завершающем этапе — сохраняем манифест во временном каталоге 
        Logger.warn(
          s"Первый носитель недоступен, итоговый манифест сохранён во временном каталоге: ${Fs.toPath(ctx.config.workDirectory).resolve(ctx.config.manifestFile)}",
          ctx.config.logFile
        )
        Right(false)
      case Left(err) => Left(err)
    stored.flatMap(onMedia => workCopy.map(_ => onMedia))

  //создаёт манифест из текущего состояния записи
  private def buildManifest(
      set: TransferSet,
      plan: Plan,
      media: List[Media],
      config: Config,
      acc: Acc,
      completed: Boolean
  ): Manifest =
    val checksums = acc.digests.completed
    Manifest(
      setId = set.id,
      name = set.name,
      createdAt = set.createdAt,
      fileCount = set.fileCount,
      totalSize = set.totalSize,
      files = PartPacker.dataOrder(filesOf(set))(_.relPath).map(f => ManifestFile(f.relPath, f.size, checksums.getOrElse(f.relPath, ""))),
      dirs = dirsOf(set),
      parts = plan.parts.map(p => p.copy(checksum = acc.partChecksums.get(p.number))),
      mediaLabels = media.map(_.label),
      mediaPaths = media.map(_.path),
      completed = completed,
      checksumAlgorithm = config.checksumAlgorithm,
      preserveEmptyDirectories = config.preserveEmptyDirectories,
      fileNameTemplate = config.fileNameTemplate
    )

  /** Завершает набор: досчитывает недостающие контрольные суммы и создаёт итоговый манифест */
  private def complete(
      set: TransferSet,
      plan: Plan,
      ctx: Ctx,
      acc: Acc
  ): Either[AppError, WriteProgress] =
    val workManifest = Fs.toPath(ctx.config.workDirectory).resolve(ctx.config.manifestFile)
    for
      _      <- checkSources(set)
      acc2   <- completeDigests(set, acc)
      manifest = buildManifest(set, plan, ctx.media, ctx.config, acc2, completed = true)
      onMedia <- storeManifest(manifest, ctx, finalCopy = true)
      _      <- SessionIO.clear(Fs.toPath(ctx.config.workDirectory))
      _       = Logger.info(s"Набор «${set.name}» полностью записан и проверен.", ctx.config.logFile)
    yield WriteProgress(
      setId = set.id,
      completedMedia = acc2.completedMedia,
      totalMedia = ctx.totalMedia,
      verifiedParts = acc2.verifiedParts,
      lastVerifiedPart = acc2.verifiedParts.lastOption,
      nextPart = plan.parts.map(_.number).find(n => !acc2.verifiedParts.contains(n)),
      manifest = manifest,
      manifestPath =
        (if onMedia then ctx.media.headOption.map(_.path).getOrElse("") else "").stripSuffix("\\") + "\\" + ctx.config.manifestFile,
      manifestOnFirstMedia = onMedia
    )

  //досчитывает недостающие контрольные суммы файлов
  private def completeDigests(set: TransferSet, acc: Acc): Either[AppError, Acc] =
    hashAll(acc.digests.missing, acc.digests.algorithm, Map.empty).map { computed =>
      acc.copy(digests = new DigestBook(acc.digests.algorithm, filesOf(set), acc.digests.completed ++ computed))
    }

  //считает контрольные суммы списка файлов
  private def hashAll(
      files: List[SourceEntry.File],
      algorithm: String,
      acc: Map[String, String]
  ): Either[AppError, Map[String, String]] =
    files match
      case Nil => Right(acc)
      case file :: tail =>
        Checksum.ofFile(Fs.toPath(file.absPath), algorithm).flatMap { sum =>
          hashAll(tail, algorithm, acc.updated(file.relPath, sum))
        }

  //сохраняет текущий прогресс записи в рабочем каталоге
  private def saveSession(set: TransferSet, ctx: Ctx, acc: Acc): Either[AppError, Unit] =
    val next = acc.verifiedParts.lastOption.map(_ + 1)
    SessionIO.save(
      SessionState(set.id, set.name, acc.completedMedia, ctx.totalMedia, acc.verifiedParts, next),
      Fs.toPath(ctx.config.workDirectory)
    )

  //проверяет, не изменились ли исходные файлы после планирования
  private def checkSources(set: TransferSet): Either[AppError, Unit] =
    val changed = filesOf(set).filter(f => !Fs.isRegularFile(f.absPath) || Fs.size(f.absPath).getOrElse(-1L) != f.size || Fs.modifiedIso(f.absPath).getOrElse("") != f.modifiedAt)
    changed.headOption match
      case None => Right(())
      case Some(f) =>
        Left(
          AppError.SourceChanged(
            f.absPath +
            (if changed.size > 1 then s"\nВсего изменённых файлов: ${changed.size}" else "") +
            "\nВыберите действие: cancel-write — отменить продолжение, rebuild — сформировать набор заново, " +
            "exclude-changed — исключить изменившиеся файлы и перестроить план."
          )
        )

  //формирует сообщение о завершении записи текущего носителя: носитель записан, следующий нужно подключить  */
  private def mediaReadyMessage(media: Media, number: Int, ctx: Ctx, partCount: Int): String =
    val head =
      s"Носитель $number из ${ctx.totalMedia} успешно записан и проверен.\n\n" +
      s"Можно безопасно отключить:\n${media.label}\n" +
      s"Частей на носителе: $partCount\n"
    ctx.media.lift(number) match
      case Some(next) => s"$head\nПодключите следующий носитель:\n${next.label}\n"
      case None       => s"$head\nВсе носители набора записаны.\n"

  //получает из набора список файлов
  private def filesOf(set: TransferSet): List[SourceEntry.File] = set.entries.collect { case f: SourceEntry.File => f }
  //получает из набора список файлов
  private def dirsOf(set: TransferSet): List[String] = set.entries.collect { case d: SourceEntry.Dir => d.relPath }

```


presentation compiler configuration:
Scala version: 3.3.8-bin-nonbootstrapped
Classpath:
<HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala3-library_3\3.3.8\scala3-library_3-3.3.8.jar [exists ], <HOME>\AppData\Local\Coursier\cache\v1\https\repo1.maven.org\maven2\org\scala-lang\scala-library\2.13.18\scala-library-2.13.18.jar [exists ]
Options:





#### Error stacktrace:

```
dotty.tools.dotc.parsing.Scanners$Region.commasExpectedInEnclosing(Scanners.scala:1655)
	dotty.tools.dotc.parsing.Scanners$Scanner.observeOutdented(Scanners.scala:720)
	dotty.tools.dotc.parsing.Parsers$Parser.statSepOrEnd(Parsers.scala:384)
	dotty.tools.dotc.parsing.Parsers$Parser.templateStatSeq$$anonfun$1(Parsers.scala:4381)
	dotty.tools.dotc.parsing.Parsers$Parser.checkNoEscapingPlaceholders(Parsers.scala:535)
	dotty.tools.dotc.parsing.Parsers$Parser.templateStatSeq(Parsers.scala:4384)
	dotty.tools.dotc.parsing.Parsers$Parser.$anonfun$42(Parsers.scala:4259)
	dotty.tools.dotc.parsing.Parsers$Parser.enclosed(Parsers.scala:591)
	dotty.tools.dotc.parsing.Parsers$Parser.inBracesOrIndented(Parsers.scala:621)
	dotty.tools.dotc.parsing.Parsers$Parser.inDefScopeBraces(Parsers.scala:627)
	dotty.tools.dotc.parsing.Parsers$Parser.templateBody(Parsers.scala:4259)
	dotty.tools.dotc.parsing.Parsers$Parser.templateBodyOpt(Parsers.scala:4252)
	dotty.tools.dotc.parsing.Parsers$Parser.template(Parsers.scala:4229)
	dotty.tools.dotc.parsing.Parsers$Parser.templateOpt(Parsers.scala:4241)
	dotty.tools.dotc.parsing.Parsers$Parser.objectDef(Parsers.scala:3952)
	dotty.tools.dotc.parsing.Parsers$Parser.tmplDef(Parsers.scala:3908)
	dotty.tools.dotc.parsing.Parsers$Parser.defOrDcl(Parsers.scala:3688)
	dotty.tools.dotc.parsing.Parsers$Parser.topStatSeq(Parsers.scala:4316)
	dotty.tools.dotc.parsing.Parsers$Parser.topstats$1(Parsers.scala:4510)
	dotty.tools.dotc.parsing.Parsers$Parser.topstats$1(Parsers.scala:4504)
	dotty.tools.dotc.parsing.Parsers$Parser.compilationUnit$$anonfun$1(Parsers.scala:4515)
	dotty.tools.dotc.parsing.Parsers$Parser.checkNoEscapingPlaceholders(Parsers.scala:535)
	dotty.tools.dotc.parsing.Parsers$Parser.compilationUnit(Parsers.scala:4520)
	dotty.tools.dotc.parsing.Parsers$Parser.parse(Parsers.scala:200)
	dotty.tools.dotc.parsing.Parser.parse$$anonfun$1(ParserPhase.scala:32)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	dotty.tools.dotc.core.Phases$Phase.monitor(Phases.scala:474)
	dotty.tools.dotc.parsing.Parser.parse(ParserPhase.scala:40)
	dotty.tools.dotc.parsing.Parser.$anonfun$2(ParserPhase.scala:52)
	scala.collection.Iterator$$anon$6.hasNext(Iterator.scala:489)
	scala.collection.Iterator$$anon$9.hasNext(Iterator.scala:593)
	scala.collection.immutable.List.prependedAll(List.scala:152)
	scala.collection.immutable.List$.from(List.scala:674)
	scala.collection.immutable.List$.from(List.scala:671)
	scala.collection.IterableOps$WithFilter.map(Iterable.scala:901)
	dotty.tools.dotc.parsing.Parser.runOn(ParserPhase.scala:51)
	dotty.tools.dotc.Run.runPhases$1$$anonfun$1(Run.scala:351)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:15)
	scala.runtime.function.JProcedure1.apply(JProcedure1.java:10)
	scala.collection.ArrayOps$.foreach$extension(ArrayOps.scala:1324)
	dotty.tools.dotc.Run.runPhases$1(Run.scala:344)
	dotty.tools.dotc.Run.compileUnits$$anonfun$1(Run.scala:384)
	dotty.tools.dotc.Run.compileUnits$$anonfun$adapted$1(Run.scala:393)
	dotty.tools.dotc.util.Stats$.maybeMonitored(Stats.scala:69)
	dotty.tools.dotc.Run.compileUnits(Run.scala:393)
	dotty.tools.dotc.Run.compileSources(Run.scala:297)
	dotty.tools.dotc.interactive.InteractiveDriver.run(InteractiveDriver.scala:180)
	dotty.tools.pc.CachingDriver.run(CachingDriver.scala:56)
	dotty.tools.pc.HoverProvider$.hover(HoverProvider.scala:43)
	dotty.tools.pc.ScalaPresentationCompiler.hover$$anonfun$1(ScalaPresentationCompiler.scala:463)
	scala.meta.internal.pc.CompilerAccess.withSharedCompiler(CompilerAccess.scala:149)
	scala.meta.internal.pc.CompilerAccess.withNonInterruptableCompiler$$anonfun$1(CompilerAccess.scala:133)
	scala.meta.internal.pc.CompilerAccess.onCompilerJobQueue$$anonfun$1(CompilerAccess.scala:210)
	scala.meta.internal.pc.CompilerJobQueue$Job.run(CompilerJobQueue.scala:153)
	java.base/java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1144)
	java.base/java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:642)
	java.base/java.lang.Thread.run(Thread.java:1583)
```
#### Short summary: 

java.lang.NullPointerException: Cannot invoke "dotty.tools.dotc.parsing.Scanners$Region.commasExpectedInEnclosing()" because the return value of "dotty.tools.dotc.parsing.Scanners$Indented.outer()" is null