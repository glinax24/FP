package transfer.operations

import transfer.domain.*

import java.nio.file.{Files, Path}

//Запись и чтение манифеста и служебных описаний носителей
object ManifestIO:

  //записывает Manifest в файл manifest.json
  def writeManifest(manifest: Manifest, dir: Path, fileName: String = "manifest.json"): Either[AppError, Unit] =
    try
      Files.createDirectories(dir)
      Files.writeString(dir.resolve(fileName), ujson.write(manifestToJson(manifest), indent = 2))
      Right(())
    catch
      case e: Exception => Left(AppError.ManifestCorrupted(s"Не удалось записать манифест: ${e.getMessage}"))

  //читает manifest.json и преобразует JSON обратно в Manifest
  def readManifest(file: Path): Either[AppError, Manifest] =
    try jsonToManifest(ujson.read(Files.readString(file)))
    catch
      case e: Exception => Left(AppError.ManifestCorrupted(e.getMessage))

  //записывает описание конкретного носителя в JSON-файл на самом носителе
  def writeDescriptor(root: Path, descriptor: MediaDescriptor, fileName: String): Either[AppError, Unit] =
    try
      Files.createDirectories(root)
      Files.writeString(root.resolve(fileName), ujson.write(descriptorToJson(descriptor), indent = 2))
      Right(())
    catch
      case e: Exception => Left(AppError.MediaNotWritable(s"${root} (не удалось записать описание носителя: ${e.getMessage})"))

  //читает описание носителя. Если файла нет — это нормально, возвращает None
  def readDescriptor(root: Path, fileName: String): Either[AppError, Option[MediaDescriptor]] =
    val file = root.resolve(fileName)
    if !Files.exists(file) then Right(None)
    else
      try Right(Some(jsonToDescriptor(ujson.read(Files.readString(file)))))
      catch
        case e: Exception => Left(AppError.ManifestCorrupted(s"описание носителя ${root} повреждено: ${e.getMessage}"))

  //превращает MediaDescriptor(описание конкретного носителя) в JSON
  def descriptorToJson(d: MediaDescriptor): ujson.Obj =
    ujson.Obj(
      "setId" -> d.setId,
      "name" -> d.name,
      "mediaNumber" -> d.mediaNumber.toDouble,
      "mediaTotal" -> d.mediaTotal.toDouble,
      "parts" -> ujson.Arr.from(d.partNumbers.map(_.toDouble)),
      "completed" -> d.completed
    )

  //превращает JSON в MediaDescriptor
  def jsonToDescriptor(json: ujson.Value): MediaDescriptor =
    val o = json.obj
    MediaDescriptor(
      setId = o("setId").str,
      name = o("name").str,
      mediaNumber = o("mediaNumber").num.toInt,
      mediaTotal = o("mediaTotal").num.toInt,
      partNumbers = o("parts").arr.toList.map(_.num.toInt),
      completed = o("completed").bool
    )

  //превращает Manifest в JSON
  def manifestToJson(m: Manifest): ujson.Obj =
    ujson.Obj(
      "setId" -> m.setId,
      "name" -> m.name,
      "createdAt" -> m.createdAt,
      "fileCount" -> m.fileCount.toDouble,
      "totalSize" -> m.totalSize.toDouble,
      "completed" -> m.completed,
      "mediaCount" -> m.mediaCount.toDouble,
      "mediaLabels" -> ujson.Arr.from(m.mediaLabels),
      "mediaPaths" -> ujson.Arr.from(m.mediaPaths),
      "dirs" -> ujson.Arr.from(m.dirs),
      "checksumAlgorithm" -> m.checksumAlgorithm,
      "preserveEmptyDirectories" -> m.preserveEmptyDirectories,
      "fileNameTemplate" -> m.fileNameTemplate,
      "files" -> ujson.Arr.from(
        m.files.map(f => ujson.Obj("relPath" -> f.relPath, "size" -> f.size.toDouble, "checksum" -> f.checksum))
      ),
      "parts" -> ujson.Arr.from(
        m.parts.map(p =>
          ujson.Obj(
            "number" -> p.number.toDouble,
            "fileName" -> p.fileName,
            "mediaNumber" -> p.mediaNumber.toDouble,
            "size" -> p.size.toDouble,
            "checksum" -> p.checksum.getOrElse("")
          )
        )
      )
    )

  //превращает JSON в Manifest
  def jsonToManifest(json: ujson.Value): Either[AppError, Manifest] =
    try
      val o = json.obj
      Right(
        Manifest(
          setId = o("setId").str,
          name = o("name").str,
          createdAt = o("createdAt").str,
          fileCount = o("fileCount").num.toInt,
          totalSize = o("totalSize").num.toLong,
          files = o("files").arr.toList.map(f => ManifestFile(f("relPath").str, f("size").num.toLong, f("checksum").str)),
          dirs = o("dirs").arrOpt.toList.flatten.toList.map(_.str),
          parts = o("parts").arr.toList.map(p =>
            Part(p("number").num.toInt, p("fileName").str, p("mediaNumber").num.toInt, p("size").num.toLong, p("checksum").strOpt)
          ),
          mediaLabels = o("mediaLabels").arr.toList.map(_.str),
          mediaPaths = o("mediaPaths").arr.toList.map(_.str),
          completed = o("completed").bool,
          checksumAlgorithm = o.get("checksumAlgorithm").map(_.str).getOrElse("SHA-256"),
          preserveEmptyDirectories = o.get("preserveEmptyDirectories").map(_.bool).getOrElse(true),
          fileNameTemplate = o.get("fileNameTemplate").map(_.str).getOrElse("{setName}.part{number}")
        )
      )
    catch
      case e: Exception => Left(AppError.ManifestCorrupted(e.getMessage))