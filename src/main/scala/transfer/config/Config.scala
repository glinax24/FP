package transfer.config

//Config хранит настройки, загруженные из файла конфигурации,
// в виде объекта Scala, чтобы остальные части программы могли ими пользоваться
enum OverwriteMode:
  case Forbid
  case Ask
  case Rename
  case Overwrite

object OverwriteMode:
  def fromString(s: String): Option[OverwriteMode] =
    s.trim.toLowerCase match
      case "forbid"    => Some(OverwriteMode.Forbid)//если файл уже существует, перезапись запрещается
      case "ask"       => Some(OverwriteMode.Ask)//если файл уже существует, программа спрашивает 
      //пользователя перезаписывать файл или нет
      case "rename"    => Some(OverwriteMode.Rename)//если файл уже существует, новый файл сохраняется под другим именем
      case "overwrite" => Some(OverwriteMode.Overwrite)//если файл уже существует, он заменяется новым
      case _           => None

//Полная конфигурация приложения
case class Config(
    workDirectory: String,
    logFile: String,
    defaultPartSize: Long,
    minimumPartSize: Long,
    reservedSpacePerMedia: Long,
    verifyAfterWrite: Boolean,
    checksumAlgorithm: String,
    overwriteMode: OverwriteMode,
    preserveEmptyDirectories: Boolean,
    fileNameTemplate: String,
    mediaDescriptorFile: String,
    manifestFile: String
)