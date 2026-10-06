package transfer.config

import transfer.domain.AppError
import transfer.util.SizeFormat

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

//Загрузка и проверка конфигурационного файла 
object ConfigLoader:

  // Конфигурация по умолчанию, если файл отсутствует
  val default: Config = Config(
    workDirectory = "transfer-work", //рабочая папка
    logFile = "transfer-manager.log",//файл журнала
    defaultPartSize = 2L * SizeFormat.GiB,//2L число типа long, SizeFormat.GiB-колво байт в гб(2гб дефолт размер)
    minimumPartSize = 64L * SizeFormat.MiB,//мин размер 64 мб
    reservedSpacePerMedia = 500L * SizeFormat.MiB,//резерв на каждом носителе 
    verifyAfterWrite = true,//после записи данные нужно проверять
    checksumAlgorithm = "SHA-256",//это для контрольных сумм
    overwriteMode = OverwriteMode.Ask,//если файл уже существует то программы должна спросить перезаписывать ли его 
    preserveEmptyDirectories = true,//пустые папки нужно сохранять
    fileNameTemplate = "{setName}.part{number}",//шаблон имени частей
    mediaDescriptorFile = "media-info.json",//имя файла с информацией о носителе 
    manifestFile = "manifest.json"
  )

  // Пытается прочитать файл, затем ресурс, затем использует конфигурацию по умолчанию
  def resolve(path: Path): Either[AppError, Config] =
    load(path) match
      case r @ Right(_) => r
      case Left(_) =>
        loadResource("transfer.json") match
          case r @ Right(_) => r
          case Left(_) =>
            loadResource("default-config.json") match
              case r @ Right(_) => r
              case Left(_)      => Right(default)

  //
  // Перезагрузка конфигурации по команде пользователя: если файл есть, но он неверен,
  // приложение сообщает об ошибке, а не переходит молча к другой конфигурации .
  def reload(path: Path): Either[AppError, Config] =
    if !Files.exists(path) then resolve(path)
    else load(path)

  //Читает конфигурацию из файла с проверкой значений
  
  def load(path: Path): Either[AppError, Config] =
    if !Files.exists(path) then Left(AppError.InvalidConfig(s"Файл конфигурации не найден: $path"))
      try
        for
          json   <- Right(ujson.read(Files.readString(path))) //читаем файл как текст, превращаем его в строку а затем в json 
          parsed <- parse(json) //метод parse берет json и создает Config()
          valid  <- validate(parsed)//полученный Config проверяется
        yield valid
      catch
        case e: Exception => Left(AppError.InvalidConfig(s"Ошибка чтения конфигурации: ${e.getMessage}")) //если какая-то непредвиденная ошибка, то выходит эксепшн, чтобы вся программа не падала

  // Читает конфигурацию по имени а не по пути
  def loadResource(name: String): Either[AppError, Config] =
    val stream = getClass.getClassLoader.getResourceAsStream(name)//«Найди ресурс с таким именем и дай мне возможность его прочитать».
    if stream == null then Left(AppError.InvalidConfig(s"Ресурс конфигурации не найден: $name"))
    else
      try
        for
          json   <- Right(ujson.read(new String(stream.readAllBytes(), StandardCharsets.UTF_8)))//читаем и получаем все байты
          parsed <- parse(json)
          valid  <- validate(parsed)
        yield valid
      catch
        case e: Exception => Left(AppError.InvalidConfig(s"Ошибка чтения ресурса конфигурации: ${e.getMessage}"))
      finally stream.close()

  //ф-я - взять JSON и сделать из него Config. если значение пустое то заполняем его из default
  def parse(json: ujson.Value): Either[AppError, Config] =
    try
      val obj = json.obj //Ттут хранятся поля json. с помощью obj можно обращаться к полям json
      def getStr(k: String, deflt: String): String =
        obj.get(k).map(_.str).filter(_.nonEmpty).getOrElse(deflt) //получить строковое значение из JSON, а если его нет или оно пустое — использовать значение по умолчанию
      def getBool(k: String, deflt: Boolean): Boolean =
        obj.get(k).map(_.bool).getOrElse(deflt)
      val getSize = (k: String, deflt: Long) =>
        obj.get(k).flatMap(v => SizeFormat.parseSize(v.str)).getOrElse(deflt)
      Right(
        Config(
          workDirectory = getStr("workDirectory", default.workDirectory),
          logFile = getStr("logFile", default.logFile),
          defaultPartSize = getSize("defaultPartSize", default.defaultPartSize),
          minimumPartSize = getSize("minimumPartSize", default.minimumPartSize),
          reservedSpacePerMedia = getSize("reservedSpacePerMedia", default.reservedSpacePerMedia),
          verifyAfterWrite = getBool("verifyAfterWrite", default.verifyAfterWrite),
          checksumAlgorithm = getStr("checksumAlgorithm", default.checksumAlgorithm),
          overwriteMode = obj.get("overwriteMode").flatMap(v => OverwriteMode.fromString(v.str)).getOrElse(default.overwriteMode),
          preserveEmptyDirectories = getBool("preserveEmptyDirectories", default.preserveEmptyDirectories),
          fileNameTemplate = getStr("fileNameTemplate", default.fileNameTemplate),
          mediaDescriptorFile = getStr("mediaDescriptorFile", default.mediaDescriptorFile),
          manifestFile = getStr("manifestFile", default.manifestFile)
        )
      )
    catch case e: Exception => Left(AppError.InvalidConfig(e.getMessage))

  //Проверяет непротиворечивость значений конфигурации  
  def validate(config: Config): Either[AppError, Config] =
    if config.minimumPartSize <= 0 then Left(AppError.InvalidConfig("minimumPartSize должен быть больше 0"))
    else if config.minimumPartSize > config.defaultPartSize then
      Left(AppError.InvalidConfig("minimumPartSize не может превышать defaultPartSize"))
    else if config.reservedSpacePerMedia < 0 then
      Left(AppError.InvalidConfig("reservedSpacePerMedia не может быть отрицательным"))
    else if config.workDirectory.trim.isEmpty then //trim убирает пробелы про краям. если строка '  ', то после того как мы уберем пробелы получится'' и далее мы проверям на emty
      Left(AppError.InvalidConfig("workDirectory не может быть пустым"))
    else if config.logFile.trim.isEmpty then
      Left(AppError.InvalidConfig("logFile не может быть пустым"))
    else if config.checksumAlgorithm.trim.isEmpty then
      Left(AppError.InvalidConfig("checksumAlgorithm не может быть пустым"))
    else if !config.fileNameTemplate.contains("{setName}") || !config.fileNameTemplate.contains("{number}") then
      Left(AppError.InvalidConfig("fileNameTemplate должен содержать {setName} и {number}"))
    else if config.mediaDescriptorFile.trim.isEmpty || config.manifestFile.trim.isEmpty then
      Left(AppError.InvalidConfig("имена служебных файлов не могут быть пустыми"))
    else Right(config)