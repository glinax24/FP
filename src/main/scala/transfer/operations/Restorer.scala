package transfer.operations

import transfer.config.{Config, OverwriteMode}
import transfer.domain.*
import transfer.util.{Fs, Logger}

import java.io.{BufferedInputStream, BufferedOutputStream, InputStream, OutputStream}
import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.control.NonFatal

//хранит информацию об ошибке восстановления файла: его путь и причину
case class RestoreProblem(relPath: String, reason: String)

//хранит итоговый отчёт о восстановлении: сколько файлов восстановлено, проверено, сколько ошибок и т.д
case class RestoreReport(
    setName: String,
    targetDir: String,
    mediaCount: Int,
    partCount: Int,
    restoredFiles: Int,
    restoredBytes: Long,
    verifiedFiles: Int,
    failedFiles: Int,
    problems: List[RestoreProblem],
    status: String
)

//
 // Восстановление исходных файлов по манифесту 
// Части читаются в порядке номеров, их данные раздаются по файлам набора в том же
// порядке, в котором файлы перечислены в манифесте. Перед использованием каждой
// части проверяется её контрольная сумма, после восстановления — суммы исходных
// файлов. Носители запрашиваются по одному в порядке манифеста, части на них не удаляются.
//
object Restorer:

  private val BufferSize = 64 * 1024

  //Текущий восстанавливаемый файл: сколько байт ещё предстоит записать
  private case class Job(
      file: ManifestFile,
      target: Path,
      left: Long,
      stream: Option[OutputStream],
      skip: Boolean,
      //Уже ли записывались байты этого файла: файл может продолжаться в следующей части
      started: Boolean
  )

  //накапливает общий результат восстановления
  private case class Acc(
      restored: Int,
      bytes: Long,
      verified: Int,
      problems: List[RestoreProblem],
      finished: List[(ManifestFile, Path)]
  )

  //Файлы, подготовленные к восстановлению, и ошибки, найденные при разрешении путей
  private case class Prepared(jobs: List[Job], problems: List[RestoreProblem])

  //запускает весь процесс восстановления от подключения носителей до проверки файлов
  def restore(
      manifest: Manifest,
      mediaRoots: List[Path],
      targetDir: Path,
      config: Config,
      confirm: String => Boolean = defaultConfirm,
      request: Option[MediaRequest] = None
  ): Either[AppError, RestoreReport] =
    if !manifest.completed then Left(AppError.SetNotCompleted(manifest.name))
    else
      // Настройки, существенные для восстановления, фиксируются в манифесте 
      val settings = config.copy(
        checksumAlgorithm = manifest.checksumAlgorithm,
        preserveEmptyDirectories = manifest.preserveEmptyDirectories
      )
      Logger.info(
        s"Начало восстановления набора «${manifest.name}» в $targetDir: носителей ${manifest.mediaCount}, частей ${manifest.parts.size}",
        config.logFile
      )
      for
        roots    <- resolveMedia(manifest, mediaRoots, request, config)
        _        <- rootsMatch(manifest, roots)
        _        <- ownership(manifest, roots, settings)
        dir      <- Fs.ensureDir(targetDir)
        _        <- checkFreeSpace(dir, manifest.totalSize)
        prepared <- createJobs(manifest, dir, settings, confirm)
        jobs      = prepared.jobs
        acc0      = Acc(0, 0L, 0, prepared.problems, Nil)
        (left, acc) <- feedParts(jobs, manifest.parts.sortBy(_.number), roots, settings, acc0)
        acc2      <- closeRemaining(left, acc)
        acc3      <- createDirs(manifest, dir, settings, acc2)
        acc4       = verifyRestored(acc3, settings)
      yield report(manifest, dir, acc4)

  //
  // Носители подключаются по очереди : программа просит путь к каждому носителю
  // в порядке манифеста, поэтому записывать их одновременно не требуется.
  //
  private def resolveMedia(
      manifest: Manifest,
      mediaRoots: List[Path],
      request: Option[MediaRequest],
      config: Config
  ): Either[AppError, List[Path]] =
    request match
      case None => Right(mediaRoots)
      case Some(ask) =>
        (1 to manifest.mediaCount).foldLeft[Either[AppError, List[Path]]](Right(Nil)) { (acc, number) =>
          acc.flatMap { roots =>
            val label = manifest.mediaLabels.lift(number - 1).getOrElse(s"носитель $number")
            val planned = manifest.mediaPaths.lift(number - 1).getOrElse("")
            ask.path(number, manifest.mediaCount, Media(label, planned, 0L, 0L, writable = true)) match
              case Left(err) => Left(err)
              case Right(path) =>
                val root = Fs.toPath(path)
                if !Files.isDirectory(root) then Left(AppError.MediaUnavailable(s"носитель $number ($label): каталог $path недоступен"))
                else Right(roots :+ root)
          }
        }

  //Чужой носитель нельзя использовать для восстановления 
  private def ownership(manifest: Manifest, roots: List[Path], config: Config): Either[AppError, Unit] =
    (1 to manifest.mediaCount).foldLeft[Either[AppError, Unit]](Right(())) { (acc, number) =>
      acc.flatMap { _ =>
        roots.lift(number - 1) match
          case Some(root) => Verifier.ownership(manifest, number, root, config)
          case None        => Left(AppError.MediaUnavailable(s"носитель $number не подключён"))
      }
    }

  //Носители передаются строго в порядке манифеста: их должно быть ровно столько, сколько нужно
  private def rootsMatch(manifest: Manifest, roots: List[Path]): Either[AppError, Unit] =
    val need = manifest.mediaCount
    if roots.size != need then
      Left(AppError.MediaUnavailable(s"подключено носителей ${roots.size}, требуется $need — добавьте носители в порядке манифеста"))
    else Right(())

  // Места в каталоге восстановления должно хватать для всего набора 
  private def checkFreeSpace(targetDir: Path, required: Long): Either[AppError, Unit] =
    val available = Fs.availableSpace(targetDir)
    if available < required then Left(AppError.NotEnoughSpaceForRestore(required, available)) else Right(())

  //создаёт задания для восстановления каждого файла
  private def createJobs(manifest: Manifest, targetDir: Path, config: Config, confirm: String => Boolean): Either[AppError, Prepared] =
    val ordered = PartPacker.dataOrder(manifest.files)(_.relPath)
    loopJobs(ordered, targetDir, config, confirm, Prepared(Nil, Nil)).map(p => p.copy(jobs = p.jobs.reverse))

  //проходит по всем файлам и создаёт для них Job.
  private def loopJobs(
      files: List[ManifestFile],
      targetDir: Path,
      config: Config,
      confirm: String => Boolean,
      acc: Prepared
  ): Either[AppError, Prepared] =
    files match
      case Nil => Right(acc)
      case file :: tail =>
        val next = safeResolve(targetDir, file.relPath) match
          case Left(error)   => Prepared(acc.jobs, RestoreProblem(file.relPath, error.message) :: acc.problems)
          case Right(target) => Prepared(resolveTarget(target, file, config, confirm) :: acc.jobs, acc.problems)
        loopJobs(tail, targetDir, config, confirm, next)

  //определяет, что делать, если файл с таким именем уже существует
  private def resolveTarget(target: Path, file: ManifestFile, config: Config, confirm: String => Boolean): Job =
    val exists = Files.exists(target)
    val allowed = !exists || (config.overwriteMode match
      case OverwriteMode.Forbid    => false
      case OverwriteMode.Overwrite => true
      case OverwriteMode.Rename    => true
      case OverwriteMode.Ask       => confirm(s"Файл уже существует: $target. Перезаписать?"))
    val destination = if exists && config.overwriteMode == OverwriteMode.Rename then uniqueName(target) else target
    Job(file, destination, file.size, None, !allowed, started = false)

  //проверяет, что путь файла не выходит за пределы каталога восстановления
  private def safeResolve(targetDir: Path, relPath: String): Either[AppError, Path] =
    val resolved = targetDir.resolve(relPath).normalize()
    if !resolved.startsWith(targetDir.normalize()) then Left(AppError.PathConflict(relPath)) else Right(resolved)

  //создаёт новое имя файла, если исходное уже занято (file (1).txt)*/
  private def uniqueName(target: Path): Path =
    val parent = Option(target.getParent).getOrElse(target.getFileSystem.getPath("."))
    val name = target.getFileName.toString
    val dot = name.lastIndexOf('.')
    val stem = if dot > 0 then name.substring(0, dot) else name
    val ext = if dot > 0 then name.substring(dot) else ""
    def attempt(index: Int): Path =
      val candidate = parent.resolve(s"$stem ($index)$ext")
      if Files.exists(candidate) then attempt(index + 1) else candidate
    attempt(1)

  //Читает части по порядку номеров и раздаёт их данные по файлам набора
  private def feedParts(
      jobs: List[Job],
      parts: List[Part],
      mediaRoots: List[Path],
      config: Config,
      acc: Acc
  ): Either[AppError, (List[Job], Acc)] =
    parts match
      case Nil => Right((jobs, acc))
      case part :: tail =>
        mediaRoots.lift(part.mediaNumber - 1) match
          case None => Left(AppError.MediaUnavailable(s"носитель ${part.mediaNumber} для части ${part.number} не подключён"))
          case Some(root) =>
            // Перед использованием каждой части проверяем, что она записана целиком (§10).
            val read =
              Verifier.verifyPart(part, root, config).flatMap { _ =>
                val in = new BufferedInputStream(Files.newInputStream(root.resolve(part.fileName)), BufferSize)
                try fill(jobs, in, part.size, acc)
                finally in.close()
              }
            read.flatMap((rest, acc2) => feedParts(rest, tail, mediaRoots, config, acc2))

  //распределяет данные одной части между текущими файлами
  private def fill(jobs: List[Job], in: InputStream, partLeft: Long, acc: Acc): Either[AppError, (List[Job], Acc)] =
    if partLeft <= 0 then Right((jobs, acc))
    else
      jobs match
        case Nil => Right((Nil, acc))
        case job :: tail =>
          if job.left <= 0 then finishJob(job, acc).flatMap(acc2 => fill(tail, in, partLeft, acc2))
          else
            val take = Math.min(job.left, partLeft)
            writeBytes(job, in, take).flatMap { updated =>
              if updated.left <= 0 then finishJob(updated, acc).flatMap(acc2 => fill(tail, in, partLeft - take, acc2))
              else Right((updated :: tail, acc))
            }

  //записывает нужное количество байт в восстанавливаемый файл
  private def writeBytes(job: Job, in: InputStream, count: Long): Either[AppError, Job] =
    if job.skip then Right(job.copy(left = job.left - count))
    else
      val opened = job.stream match
        case Some(stream) => Right((stream, job))
        case None         => openStream(job).map(stream => (stream, job.copy(stream = Some(stream), started = true)))
      opened.flatMap { (out, current) =>
        val buffer = new Array[Byte](BufferSize)
        try copyN(out, in, count, buffer).map(_ => current.copy(left = current.left - count))
        catch case NonFatal(e) => Left(AppError.SourceUnreadable(s"${job.file.relPath} (${e.getMessage})"))
      }

  //
   //Первый фрагмент файла создаёт его заново (или перезаписывает, если разрешено),
   // а последующие дописывают данные: файл может быть разбит на несколько частей 
  //
  private def openStream(job: Job): Either[AppError, OutputStream] =
    try
      Option(job.target.getParent).foreach(Files.createDirectories(_))
      val options =
        if job.started then Array(StandardOpenOption.WRITE, StandardOpenOption.APPEND)
        else Array(StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING)
      Right(new BufferedOutputStream(Files.newOutputStream(job.target, options*), BufferSize))
    catch case NonFatal(e) => Left(AppError.SourceUnreadable(s"${job.file.relPath} (${e.getMessage})"))

  //копирует заданное количество байт из одной части в файл
  private def copyN(out: OutputStream, in: InputStream, count: Long, buffer: Array[Byte]): Either[AppError, Unit] =
    var left = count
    var result: Either[AppError, Unit] = Right(())
    while left > 0 && result.isRight do
      val read = in.read(buffer, 0, Math.min(buffer.length.toLong, left).toInt)
      if read <= 0 then result = Left(AppError.SizeMismatch("данные частей", count, count - left))
      else
        out.write(buffer, 0, read)
        left -= read
    result

  //завершает восстановление одного файла и добавляет его в результат
  private def finishJob(job: Job, acc: Acc): Either[AppError, Acc] =
    if job.skip then Right(problem(job.file.relPath, "файл уже существует, перезапись запрещена", acc))
    else
      try
        job.stream.foreach(_.close())
        // Файл нулевой длины не открывался для записи, но должен быть создан.
        // Каталог к этому моменту мог ещё не появиться — создаём его явно.
        if job.file.size == 0 && !Files.exists(job.target) then
          Option(job.target.getParent).foreach(Files.createDirectories(_))
          Files.createFile(job.target)
        Right(acc.copy(restored = acc.restored + 1, bytes = acc.bytes + job.file.size, finished = (job.file, job.target) :: acc.finished))
      catch case NonFatal(e) => Right(problem(job.file.relPath, s"не удалось завершить запись: ${e.getMessage}", acc))

  //обрабатывает файлы, до которых данные не дошли
  private def closeRemaining(jobs: List[Job], acc: Acc): Either[AppError, Acc] =
    jobs match
      case Nil => Right(acc)
      case job :: tail =>
        closeQuietly(job)
        val next =
          if job.skip then problem(job.file.relPath, "файл уже существует, перезапись запрещена", acc)
          else if job.left > 0 then problem(job.file.relPath, "восстановлено меньше данных, чем указано в манифесте", acc)
          else
            try
              if !Files.exists(job.target) then Files.createFile(job.target)
              acc.copy(restored = acc.restored + 1, bytes = acc.bytes + job.file.size, finished = (job.file, job.target) :: acc.finished)
            catch case NonFatal(e) => problem(job.file.relPath, s"не удалось создать файл: ${e.getMessage}", acc)
        closeRemaining(tail, next)

  //создаёт пустые каталоги, если это разрешено настройками
  private def createDirs(manifest: Manifest, targetDir: Path, config: Config, acc: Acc): Either[AppError, Acc] =
    if !config.preserveEmptyDirectories then Right(acc) else loopDirs(manifest.dirs, targetDir, acc)

  //проходит по списку каталогов и создаёт их
  private def loopDirs(dirs: List[String], targetDir: Path, acc: Acc): Either[AppError, Acc] =
    dirs match
      case Nil => Right(acc)
      case rel :: tail =>
        val next = safeResolve(targetDir, rel) match
          case Left(error) => problem(rel, error.message, acc)
          case Right(dir) =>
            try
              Files.createDirectories(dir)
              acc
            catch case NonFatal(e) => problem(rel, s"не удалось создать каталог: ${e.getMessage}", acc)
        loopDirs(tail, targetDir, next)

  //проверяет контрольные суммы восстановленных файлов
  private def verifyRestored(acc: Acc, config: Config): Acc =
    loopFiles(acc.finished, acc, config)

  //проверяет каждый восстановленный файл
  private def loopFiles(files: List[(ManifestFile, Path)], acc: Acc, config: Config): Acc =
    files match
      case Nil => acc
      case (file, path) :: tail =>
        val next =
          if Files.exists(path) then
            Verifier.verifyFile(path, file.size, file.checksum, config.checksumAlgorithm) match
              case Right(_)    => acc.copy(verified = acc.verified + 1)
              case Left(error) => problem(file.relPath, error.message, acc)
          else problem(file.relPath, "файл не создан", acc)
        loopFiles(tail, next, config)

  //добавляет ошибку в список проблем
  private def problem(relPath: String, reason: String, acc: Acc): Acc =
    acc.copy(problems = RestoreProblem(relPath, reason) :: acc.problems)

  //закрывает поток без выброса ошибки
  private def closeQuietly(job: Job): Unit =
    try job.stream.foreach(_.close())
    catch case NonFatal(_) => ()

  // Отчёт о восстановлении собирается из накопленных итогов
  private def report(manifest: Manifest, targetDir: Path, acc: Acc): RestoreReport =
    val status = if acc.problems.isEmpty then "успешно" else "завершено с ошибками"
    Logger.info(s"Восстановление набора «${manifest.name}»: файлов ${acc.restored}, ошибок ${acc.problems.size}.")
    RestoreReport(
      setName = manifest.name,
      targetDir = targetDir.toString,
      mediaCount = manifest.mediaCount,
      partCount = manifest.parts.size,
      restoredFiles = acc.restored,
      restoredBytes = acc.bytes,
      verifiedFiles = acc.verified,
      failedFiles = acc.problems.size,
      problems = acc.problems.reverse,
      status = status
    )

  //спрашивает пользователя, можно ли перезаписать существующий файл
  private def defaultConfirm(question: String): Boolean =
    val console = System.console()
    console != null && {
      val answer = console.readLine(s"$question [y/N] ")
      answer != null && Set("y", "yes", "д", "да").contains(answer.trim.toLowerCase)
    }
