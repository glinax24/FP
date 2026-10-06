package transfer.operations

import transfer.domain.*

// PartPacker отвечает за раскладку данных файлов по частям согласно плану

//Фрагмент исходного файла, попадающий в часть: файл может быть разбит на части
case class FileSlice(relPath: String, sourcePath: String, offset: Long, length: Long)

// Содержимое одной части:её номер, имя, размер и список фрагментов 
case class PartContent(number: Int, fileName: String, size: Long, slices: List[FileSlice])

//
//Чистое распределение данных набора по частям согласно готовому плану .
// Пользователь не выбирает, что попадёт на конкретный носитель: распределение
// считает программа
//
object PartPacker:

  //
   // Порядок, в котором файлы занимают байты внутри части : по относительному пути.
   // Манифест и восстановление обязаны использовать тот же порядок, иначе при обратной
   // сборке байты попадут в другие файлы и контрольные суммы не сойдутся.
   //Пустые файлы байтов не занимают, но остаются в списке — их нужно восстановить.
   //
  
  //ортирует файлы по относительному пути, задавая порядок их записи в части
  def dataOrder[T](files: List[T])(relPathOf: T => String): List[T] =
    files.sortBy(relPathOf)

  //оставляет только непустые файлы и сортирует их для распределения по частям
  private def packedOrder(files: List[SourceEntry.File]): List[SourceEntry.File] =
    dataOrder(files.filter(_.size > 0))(_.relPath)

  //хранит текущее состояние распределения данных: оставшиеся файлы, текущий файл и количество оставшихся байт
  private case class Stream(rest: List[SourceEntry.File], current: Option[SourceEntry.File], left: Long)

  //распределяет все файлы по частям согласно готовому плану
  def pack(files: List[SourceEntry.File], parts: List[Part]): Either[AppError, List[PartContent]] =
    val planBytes = parts.map(_.size).sum
    val dataBytes = files.map(_.size).sum
    if dataBytes != planBytes then
      Left(AppError.SourceChanged(s"объём набора изменился после планирования: в плане $planBytes, сейчас $dataBytes"))
    else
      Right(fillAll(parts, Stream(packedOrder(files), None, 0L)))

  //распределяет данные по новому плану, пропуская уже записанные байты
  def packFrom(files: List[SourceEntry.File], parts: List[Part], skip: Long): Either[AppError, List[PartContent]] =
    if skip < 0 then Left(AppError.SourceChanged(s"неверная точка продолжения: $skip"))
    else
      val ordered = packedOrder(files)
      val remain = ordered.map(_.size).sum - skip
      val planBytes = parts.map(_.size).sum
      if remain != planBytes then
        Left(AppError.SourceChanged(s"объём остатка набора изменился после пересчёта плана: в плане $planBytes, осталось данных $remain"))
      else
        Right(fillAll(parts, skipStream(ordered, skip)))

  //пропускает указанное количество уже записанных байт и определяет, с какого места продолжить
  private def skipStream(files: List[SourceEntry.File], skip: Long): Stream =
    files match
      case Nil          => Stream(Nil, None, 0L)
      case file :: tail =>
        if skip <= 0L then Stream(tail, Some(file), file.size)
        else if file.size > skip then Stream(tail, Some(file), file.size - skip)
        else skipStream(tail, skip - file.size)

  //последовательно заполняет все части данными
  private def fillAll(parts: List[Part], stream: Stream): List[PartContent] =
    parts match
      case Nil => Nil
      case part :: tail =>
        val (slices, rest) = take(part.size, stream)
        PartContent(part.number, part.fileName, part.size, slices) :: fillAll(tail, rest)

  //берёт нужное количество байт из текущих файлов и формирует соответствующие FileSlice
  private def take(bytes: Long, stream: Stream): (List[FileSlice], Stream) =
    if bytes <= 0 then (Nil, stream)
    else
      stream.current match
        case None =>
          stream.rest match
            case Nil          => (Nil, stream)
            case file :: tail => take(bytes, Stream(tail, Some(file), file.size))
        case Some(file) =>
          if stream.left <= 0 then take(bytes, Stream(stream.rest, None, 0L))
          else
            val len = Math.min(bytes, stream.left)
            val slice = FileSlice(file.relPath, file.absPath, file.size - stream.left, len)
            val (slices, rest) = take(bytes - len, Stream(stream.rest, stream.current, stream.left - len))
            (slice :: slices, rest)