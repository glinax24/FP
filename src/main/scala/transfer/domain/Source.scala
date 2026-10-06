package transfer.domain

//Хранит объект, который пользователь выбрал для переноса: файл или директорию
enum SelectedItem:
  case File(absPath: String, relPath: String)
  case Directory(absPath: String, relPath: String)

  // Возвращает абсолютный путь выбранного объекта
  def absPathOf: String = this match
    case File(p, _)      => p
    case Directory(p, _) => p

  //Проверяет, является ли выбранный объект директорией
  def isDir: Boolean = this match
    case Directory(_, _) => true
    case _               => false

//Описывает элемент набора после обработки выбранного объекта
enum SourceEntry:
  case File(absPath: String, relPath: String, size: Long, modifiedAt: String, readable: Boolean)
  case Dir(absPath: String, relPath: String)

  // Возвращает относительный путь элемента
  def relPathOf: String = this match
    case File(_, rel, _, _, _) => rel
    case Dir(_, rel)           => rel

   //Возвращает абсолютный путь элемента
  def absPathOf: String = this match
    case File(abs, _, _, _, _) => abs
    case Dir(abs, _)           => abs

  // Проверяет, является ли элемент файлом
  def isFile: Boolean = this match
    case File(_, _, _, _, _) => true
    case _                   => false


  //Возвращает размер файла или 0 для директории
  def sizeOf: Long = this match
    case File(_, _, size, _, _) => size
    case Dir(_, _)             => 0L

//Хранит информацию о наборе данных, который будет переноситься: 
  //имя, ID, файлы, директории, общий размер и найденные проблемы
case class TransferSet(
    id: String,
    name: String,
    createdAt: String,
    entries: List[SourceEntry],
    totalSize: Long,
    fileCount: Int,
    directoryCount: Int,
    unreadable: List[String],
    pathConflicts: List[String],
    //
     // Файлы, исключённые из набора из-за конфликта относительных путей :
     // под одним именем может восстановиться только один файл.
    dropped: List[String] = Nil
)

object TransferSet:
   // Генерирует короткий уникальный идентификатор набора
  def freshId: String = java.util.UUID.randomUUID().toString.replace("-", "").take(6)
  // Возвращает текущее время в формате ISO
  def nowIso: String = java.time.Instant.now().toString