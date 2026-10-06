package transfer.domain

//Часть переносимого набора 
case class Part(
    number: Int,
    fileName: String,
    mediaNumber: Int,
    size: Long,
    // Контрольная сумма известна только после записи части, на этапе плана — пусто
    checksum: Option[String] = None
)

//Запись об исходном файле в манифесте
case class ManifestFile(
    relPath: String,
    size: Long,
    checksum: String
)

// Манифест переносимого набора 
case class Manifest(
    setId: String, //идентификатор всего переноса
    name: String, //имя набора
    createdAt: String, //дата и время создания этой записи
    fileCount: Int,//колво файлов нутри набора
    totalSize: Long,//суммарный размер всех файлов
    files: List[ManifestFile], //список всех исходный файлов
    // Относительные пути каталогов набора: нужны, чтобы восстановить и пустые каталоги 
    dirs: List[String],//список всех папок, чтобы воссоздать структуру каталогов, даже если папка пустая
    parts: List[Part],//список всех нарезанных кусочков (описанных в классе part
    mediaPaths: List[String],//пути к этим дискам в системе во время записи
    mediaLabels: List[String],//метки/названия дисков (например, ["FLASH_1", "FLASH_2"]).
    completed: Boolean,//флаг: завершена ли полностью запись всего набора (true/false)
    /// Настройки, существенные для восстановления: фиксируются при записи 
    checksumAlgorithm: String = "SHA-256",//алгоритм вычисления отпечатков файлов
    preserveEmptyDirectories: Boolean = true,//настройка нужно ли сохранять пустые папки(по дефолту-да)
    fileNameTemplate: String = "{setName}.part{number}"//шаблон, по которому будут называться файлы-кусочки на диске
):
  //это функция, которая считает количество физических носителей
    def mediaCount: Int = mediaLabels.size

//
// Служебное описание носителя: позволяет определить, к какому набору он относится,
// какой это номер носителя, какие части на нём лежат и завершена ли его запись.

case class MediaDescriptor(
    setId: String, //к какому общему набору данных принадлежит эта флешка
    name: String,//имя этого набора
    mediaNumber: Int,//номер этой конкретной флешки 
    mediaTotal: Int,//сколько всего флешек в этом наборе
    partNumbers: List[Int],//список номеров тех частей (Part), которые записаны именно на эту флешку
    completed: Boolean//флаг: закончена ли запись конкретно на этот носитель
)