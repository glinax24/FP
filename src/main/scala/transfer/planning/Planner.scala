package transfer.planning

import transfer.config.Config
import transfer.domain.*
import transfer.util.{Logger, SizeFormat}

//Как разместить данные по частям и носителям
//Он учитывает размер данных;размер носителей;
//минимальный и максимальный размер части;
//зарезервированное место;
//ограничения файловой системы
object Planner:

  // Место под служебное описание носителя на каждом носителе  
  val MediaDescriptorOverhead: Long = 64L * 1024

  // Место под манифест на первом носителе 
  val ManifestOverhead: Long = SizeFormat.MiB

  // Ограничение размера одного файла для типичной файловой системы носителя (FAT32 = 4 ГБ)
  val FileSizeLimit: Long = 4L * SizeFormat.GiB

  //определяет размер одной части с учётом максимального и минимального размера и свободного места
  def computePartSize(mediaUsable: Long, defaultMax: Long, minimum: Long): Long =
    if mediaUsable < minimum then minimum
    else Math.min(defaultMax, mediaUsable)

  //создаёт имя части по шаблону из конфигурации
  def partFileName(template: String, setName: String, number: Int, total: Int = 0): String =
    val width = Math.max(numberWidth(total), String.valueOf(number).length)
    val padded = java.lang.String.format("%0" + width + "d", Int.box(number))
    template.replace("{setName}", setName).replace("{number}", padded)

  //определяет, сколько цифр использовать в номере части
  private def numberWidth(total: Int): Int = Math.max(3, String.valueOf(Math.max(1, total)).length)

  // Количество частей, на которое разобьётся объём при заданном размере части
  private def partCountOf(totalSize: Long, partSize: Long): Int =
    if totalSize <= 0 then 0 else Math.ceil(totalSize.toDouble / Math.max(1L, partSize).toDouble).toInt

  //
   // Восстанавливает план разбиения из манифеста: после перезапуска программы план
   // можно не перестраивать, размеры частей и их носители уже зафиксированы.
  def planFromManifest(manifest: Manifest): Plan =
    val parts = manifest.parts
    val partSize = parts.map(_.size).filter(_ > 0).minOption.getOrElse(0L)
    val allocations = manifest.mediaPaths.indices.toList.map { index =>
      val number = index + 1
      val mine = parts.filter(_.mediaNumber == number)
      MediaAllocation(number, mine.map(_.number), mine.map(_.size).sum, 0L)
    }
    Plan(manifest.setId, manifest.name, manifest.totalSize, partSize, parts, allocations, Nil, manifest.mediaLabels)

  //считает доступное место на носителе после вычета резерва и служебных файлов
  def capacityOf(media: Media, mediaNumber: Int, config: Config): Long =
    Math.max(0L, Media.usableSpace(media, config.reservedSpacePerMedia) - serviceOverhead(mediaNumber))

  //определяет размер места под служебные файлы конкретного носителя
  private def serviceOverhead(mediaNumber: Int): Long =
    if mediaNumber == 1 then ManifestOverhead + MediaDescriptorOverhead else MediaDescriptorOverhead

  //
   // Строит план: разбивает набор на части и распределяет их по носителям
   // в заданном порядке — сначала заполняется первый носитель, затем второй и далее.
  def buildPlan(set: TransferSet, media: List[Media], config: Config): Either[AppError, Plan] =
    for
      _           <- checkWritable(media)
      _           <- checkMediaFitOnePart(media, config)
      _           <- checkTotalCapacity(media, set.totalSize, config)
      partSize     = planPartSize(media, config)
      capacities   = media.zipWithIndex.map((m, i) => (i + 1, capacityOf(m, i + 1, config)))
      (parts, allocations) = distribute(set.name, set.totalSize, partSize, capacities, 1, config, partCountOf(set.totalSize, partSize))
    yield
      Plan(
        setId = set.id,
        name = set.name,
        totalSize = set.totalSize,
        partSize = partSize,
        parts = parts,
        allocations = allocations,
        warnings = warningsFor(set, partSize, parts, allocations, config),
        mediaLabels = media.map(_.label)
      )

  //
   // Перестраивает оставшуюся часть плана: когда доступного места на очередном носителе
  // стало меньше, чем при планировании, план для непроверенных частей считается заново.
  // Уже записанные и проверенные части не изменяются, их номера и носители сохраняются.
  def replanTail(
      set: TransferSet,
      currentPlan: Plan,
      verifiedParts: List[Int],
      remainingMedia: List[Media],
      config: Config
  ): Either[AppError, Plan] =
    val pending = currentPlan.parts.filterNot(p => verifiedParts.contains(p.number))
    pending.headOption match
      case None => Right(currentPlan)
      case Some(first) =>
        val firstNumber = first.number
        val firstMediaNumber = first.mediaNumber
        val remainingBytes = pending.map(_.size).sum
        val capacities = remainingMedia.zipWithIndex.map((m, i) => (firstMediaNumber + i, capacityOf(m, firstMediaNumber + i, config)))
        val totalCapacity = capacities.map(_._2).sum
        if totalCapacity < remainingBytes then Left(AppError.NotEnoughTotalSpace(remainingBytes, totalCapacity))
        else
          val smallest = capacities.map(_._2).minOption.getOrElse(config.minimumPartSize)
          val partSize = Math.min(computePartSize(smallest, config.defaultPartSize, config.minimumPartSize), FileSizeLimit)
          val (tailParts, tailAllocs) = distribute(set.name, remainingBytes, partSize, capacities, firstNumber, config, firstNumber - 1 + partCountOf(remainingBytes, partSize))
          val fullParts = currentPlan.parts.filter(p => verifiedParts.contains(p.number)) ++ tailParts
          val fullAllocs = currentPlan.allocations.takeWhile(_.mediaNumber < firstMediaNumber) ++ tailAllocs
          Right(
            Plan(
              setId = currentPlan.setId,
              name = currentPlan.name,
              totalSize = currentPlan.totalSize,
              partSize = partSize,
              parts = fullParts,
              allocations = fullAllocs,
              warnings = warningsFor(set, partSize, fullParts, fullAllocs, config),
              mediaLabels = currentPlan.mediaLabels
            )
          )

  //проверяет, что все носители доступны для записи
  private def checkWritable(media: List[Media]): Either[AppError, Unit] =
    media.collectFirst { case m if !m.writable => m } match
      case Some(m) => Left(AppError.MediaNotWritable(m.path))
      case None    => Right(())

  //проверяет, что каждый носитель вмещает хотя бы минимальную часть
  private def checkMediaFitOnePart(media: List[Media], config: Config): Either[AppError, Unit] =
    media.zipWithIndex.collectFirst {
      case (m, i) if capacityOf(m, i + 1, config) < config.minimumPartSize =>
        (i + 1, m, capacityOf(m, i + 1, config))
    } match
      case Some((number, m, capacity)) =>
        val error = AppError.NotEnoughSpaceOnMedia(s"${m.path} (носитель $number)", config.minimumPartSize, capacity)
        Logger.error(s"Нехватка места: ${error.message}", config.logFile)
        Left(error)
      case None => Right(())

  // Если суммарного объёма носителей недостаточно, запись не начинается 
  private def checkTotalCapacity(media: List[Media], totalSize: Long, config: Config): Either[AppError, Unit] =
    val total = capacitySum(media, config)
    if total < totalSize then
      val error = AppError.NotEnoughTotalSpace(totalSize, total)
      Logger.error(s"Нехватка места: ${error.message}", config.logFile)
      Left(error)
    else Right(())

  //считает суммарную доступную ёмкость всех носителей
  private def capacitySum(media: List[Media], config: Config): Long =
    media.zipWithIndex.map((m, i) => capacityOf(m, i + 1, config)).sum

  //определяет размер части с учётом самого маленького носителя и ограничения файловой системы
  private def planPartSize(media: List[Media], config: Config): Long =
    val smallestCapacity =
      media.zipWithIndex.map((m, i) => capacityOf(m, i + 1, config)).minOption.getOrElse(config.minimumPartSize)
    Math.min(computePartSize(smallestCapacity, config.defaultPartSize, config.minimumPartSize), FileSizeLimit)

  //распределяет весь объём данных по носителям и создаёт список частей
  private def distribute(
      setName: String,
      totalSize: Long,
      partSize: Long,
      capacities: List[(Int, Long)],
      firstNumber: Int,
      config: Config,
      partCount: Int
  ): (List[Part], List[MediaAllocation]) =
    // рекурсивно распределяет части данных по носителям, переходя к следующему носителю после заполнения текущего
    def loop(
        nextNumber: Int,
        rest: Long,
        caps: List[(Int, Long)],
        partsAcc: List[Part],
        allocsAcc: List[MediaAllocation]
    ): (List[Part], List[MediaAllocation]) =
      caps match
        case Nil => (partsAcc.reverse, allocsAcc.reverse)
        case (number, capacity) :: tail =>
          val toPlace = Math.min(capacity, rest)
          val (parts, written) = fillMedia(number, toPlace, partSize, nextNumber, setName, config, partCount)
          val allocation = MediaAllocation(number, parts.map(_.number), written, Math.max(0L, capacity - written))
          loop(nextNumber + parts.size, rest - written, tail, parts.reverse ::: partsAcc, allocation :: allocsAcc)

    loop(firstNumber, totalSize, capacities, Nil, Nil)

  //заполняет один носитель частями, пока не закончится место или данные
  private def fillMedia(
      mediaNumber: Int,
      dataBytes: Long,
      partSize: Long,
      firstNumber: Int,
      setName: String,
      config: Config,
      partCount: Int
  ): (List[Part], Long) =
    def loop(number: Int, left: Long, acc: List[Part]): (List[Part], Long) =
      if left <= 0 then (acc.reverse, dataBytes - left)
      else
        val size = Math.min(partSize, left)
        val part = Part(number, partFileName(config.fileNameTemplate, setName, number, partCount), mediaNumber, size, None)
        loop(number + 1, left - size, part :: acc)
    loop(firstNumber, dataBytes, Nil)

  //формирует предупреждения о проблемах или особенностях полученного плана
  private def warningsFor(
      set: TransferSet,
      partSize: Long,
      parts: List[Part],
      allocations: List[MediaAllocation],
      config: Config
  ): List[String] =
    val fsLimited =
      if config.defaultPartSize > FileSizeLimit then
        List(s"Размер части ограничен ограничением файловой системы: ${SizeFormat.humanReadable(FileSizeLimit)}")
      else Nil
    val unused = allocations.collect { case a if a.partNumbers.isEmpty => s"Носитель ${a.mediaNumber} остаётся свободным" }
    val tail =
      if parts.sizeIs > 1 && parts.lastOption.exists(_.size < partSize) then List("Последняя часть заполнена не полностью")
      else Nil
    val empty = if set.fileCount == 0 then List("В наборе нет файлов для переноса") else Nil
    fsLimited ++ unused ++ tail ++ empty