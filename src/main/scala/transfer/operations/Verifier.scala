package transfer.operations

import transfer.config.Config
import transfer.domain.*
import transfer.util.{Checksum, Fs, Logger}

import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*
import scala.util.control.NonFatal

//Здесь Verifier отвечает за проверку записанного набора и его частей

//результат проверки одного носителя: его номер, название, успешность и список ошибок
case class VerifyMediaResult(mediaNumber: Int, label: String, ok: Boolean, failures: List[AppError])

//итоговый отчёт о проверке всего набора
case class VerifyReport(setName: String, results: List[VerifyMediaResult], failed: Boolean)

/** Проверка частей и готового набора по манифесту  */
object Verifier:

  //
   // Проверяет набор по манифесту : принадлежность носителя набору, номер носителя,
   // наличие всех частей, их размеры и контрольные суммы, отсутствие лишних частей.
  // Носители запрашиваются по одному в порядке манифеста — подключать их все сразу не нужно.

  
  //запускает полную проверку набора по манифесту
  def verify(
      manifest: Manifest,
      mediaRoots: List[Path],
      config: Config,
      request: Option[MediaRequest] = None
  ): Either[AppError, VerifyReport] =
    if !manifest.completed then Left(AppError.SetNotCompleted(manifest.name))
    else
      Logger.info(s"Начало проверки набора «${manifest.name}»: носителей ${manifest.mediaCount}", config.logFile)
      request match
        case Some(ask) => resolveMedia(manifest, ask).map(roots => run(manifest, roots, config))
        case None      => Right(run(manifest, mediaRoots, config))


  //проверяет все носители по очереди и формирует итоговый отчёт
  private def run(manifest: Manifest, mediaRoots: List[Path], config: Config): VerifyReport =
    val results = manifest.parts.map(_.mediaNumber).distinct.sorted.map { number =>
      val label = manifest.mediaLabels.lift(number - 1).getOrElse(s"носитель $number")
      mediaRoots.lift(number - 1) match
        case None      => VerifyMediaResult(number, label, ok = false, List(AppError.MediaUnavailable("носитель не подключён")))
        case Some(root) => checkMedia(manifest, number, root, config)
    }
    val report = VerifyReport(manifest.name, results, results.exists(!_.ok))
    Logger.info(
      s"Проверка набора «${manifest.name}» завершена: " +
      (if report.failed then s"ошибок на ${report.results.count(!_.ok)} носителях" else "все носители в порядке"),
      config.logFile
    )
    report

  //по очереди запрашивает у пользователя пути к носителям и проверяет, что они доступны
  private def resolveMedia(manifest: Manifest, request: MediaRequest): Either[AppError, List[Path]] =
    (1 to manifest.mediaCount).foldLeft[Either[AppError, List[Path]]](Right(Nil)) { (acc, number) =>
      acc.flatMap { roots =>
        val label = manifest.mediaLabels.lift(number - 1).getOrElse(s"носитель $number")
        val planned = manifest.mediaPaths.lift(number - 1).getOrElse("")
        request.path(number, manifest.mediaCount, Media(label, planned, 0L, 0L, writable = true)) match
          case Left(err) => Left(err)
          case Right(path) =>
            val root = Fs.toPath(path)
            if !Files.isDirectory(root) then Left(AppError.MediaUnavailable(s"носитель $number ($label): каталог $path недоступен"))
            else Right(roots :+ root)
      }
    }

  //полностью проверяет один носитель: описание, части и лишние файлы
  def checkMedia(manifest: Manifest, number: Int, root: Path, config: Config): VerifyMediaResult =
    val label = manifest.mediaLabels.lift(number - 1).getOrElse(s"носитель $number")
    val expected = manifest.parts.filter(_.mediaNumber == number)
    val failures = descriptorFailures(manifest, number, root, config) ++ partFailures(expected, root, config) ++ extraPartFailures(expected, root, config)
    VerifyMediaResult(number, label, failures.isEmpty, failures)

  //проверяет, что носитель действительно принадлежит нужному набору
  def ownership(manifest: Manifest, number: Int, root: Path, config: Config): Either[AppError, Unit] =
    descriptorFailures(manifest, number, root, config) match
      case Nil        => Right(())
      case first :: _ =>
        Logger.error(s"Носитель $number (${manifest.mediaLabels.lift(number - 1).getOrElse("")}): ${first.message}", config.logFile)
        Left(first)

  // Часть должна существовать, иметь ожидаемый размер и совпадающую контрольную сумму ,собирает ошибки 
  private def partFailures(parts: List[Part], root: Path, config: Config): List[AppError] =
    parts
      .map(part => verifyPart(part, root, config))
      .collect { case Left(err) => err }

  //проверяет служебное описание носителя: набор, номер и завершённость записи
  private def descriptorFailures(manifest: Manifest, number: Int, root: Path, config: Config): List[AppError] =
    if !Files.isDirectory(root) then List(AppError.MediaUnavailable(root.toString))
    else
      ManifestIO.readDescriptor(root, config.mediaDescriptorFile) match
        case Left(err)   => List(err)
        case Right(None) => List(AppError.MissingState(s"носитель $number: отсутствует служебное описание ${config.mediaDescriptorFile}"))
        case Right(Some(d)) =>
          if d.setId != manifest.setId then List(AppError.WrongMedia(s"носитель $number набора ${manifest.setId}", s"носитель набора ${d.setId}"))
          else if d.mediaNumber != number then List(AppError.MediaWrongNumber(number, d.mediaNumber))
          else if !d.completed then List(AppError.MissingState(s"носитель $number: запись не завершена"))
          else Nil

  // ищетчасти, оставшиеся от других наборов или лишние части текущего 
  private def extraPartFailures(expected: List[Part], root: Path, config: Config): List[AppError] =
    val expectedNames = expected.map(_.fileName).toSet
    val service = List(config.manifestFile, config.mediaDescriptorFile)
    partFilePattern(config.fileNameTemplate) match
      case None => Nil
      case Some(pattern) =>
        try
          val stream = Files.list(root)
          try
            stream.iterator().asScala.toList
              .map(_.getFileName.toString)
              .filterNot(service.contains)
              .filter(name => pattern.matcher(name).matches())
              .filterNot(expectedNames.contains)
              .map(name => AppError.UnexpectedPart(name))
          finally stream.close()
        catch case NonFatal(_) => Nil

  // создаёт шаблон для распознавания имён файлов частей: имя набора — любой текст, номер — цифры
  private def partFilePattern(template: String): Option[java.util.regex.Pattern] =
    val setIndex = template.indexOf("{setName}")
    val numberIndex = template.indexOf("{number}")
    if setIndex < 0 || numberIndex < 0 || setIndex > numberIndex then None
    else
      val quoted = java.util.regex.Pattern.quote
      val regex =
        quoted(template.substring(0, setIndex)) +
          ".*" +
          quoted(template.substring(setIndex + "{setName}".length, numberIndex)) +
          "\\d+" +
          quoted(template.substring(numberIndex + "{number}".length))
      Some(java.util.regex.Pattern.compile("^" + regex + "$"))

  //
  // Проверяет, что часть записана полностью, и возвращает её контрольную сумму.
  // Если в части уже записана ожидаемая контрольная сумма, она сверяется с фактической.
  def verifyPart(part: Part, root: Path, config: Config): Either[AppError, String] =
    val file = root.resolve(part.fileName)
    if !Files.exists(file) then Left(AppError.PartMissing(part.number))
    else
      try
        val size = Files.size(file)
        if size != part.size then Left(AppError.SizeMismatch(part.fileName, part.size, size))
        else
          Checksum.ofFile(file, config.checksumAlgorithm).flatMap { actual =>
            part.checksum match
              case Some(expected) if !expected.equalsIgnoreCase(actual) =>
                Left(AppError.ChecksumMismatch(part.fileName, expected, actual))
              case _ => Right(actual)
          }
      catch case NonFatal(e) => Left(AppError.SourceUnreadable(s"${part.fileName} (${e.getMessage})"))

  //проверяет сразу после записи, что часть существует 
  //и имеет правильный размер, затем считает checksum
  def verifyPartWritten(part: Part, root: Path, config: Config): Either[AppError, String] =
    val file = root.resolve(part.fileName)
    if !Files.exists(file) then Left(AppError.PartMissing(part.number))
    else
      try
        val size = Files.size(file)
        if size != part.size then Left(AppError.SizeMismatch(part.fileName, part.size, size))
        else Checksum.ofFile(file, config.checksumAlgorithm)
      catch case NonFatal(e) => Left(AppError.SourceUnreadable(s"${part.fileName} (${e.getMessage})"))

  //Проверяет произвольный файл: размер и контрольную сумму
  def verifyFile(path: Path, expectedSize: Long, expectedChecksum: String, algorithm: String): Either[AppError, Unit] =
    try
      val actualSize = Files.size(path)
      if actualSize != expectedSize then
        Left(AppError.SizeMismatch(path.toString, expectedSize, actualSize))
      else
        Checksum.ofFile(path, algorithm).flatMap { actual =>
          if actual.equalsIgnoreCase(expectedChecksum) then Right(())
          else Left(AppError.ChecksumMismatch(path.toString, expectedChecksum, actual))
        }
    catch
      case NonFatal(e) =>
        Left(AppError.ChecksumMismatch(path.toString, expectedChecksum, s"не удалось прочитать: ${e.getMessage}"))
