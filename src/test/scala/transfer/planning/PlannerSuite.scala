package transfer.planning

import transfer.config.ConfigLoader
import transfer.domain.*

class PlannerSuite extends munit.FunSuite:

  private val MiB = 1024L * 1024
  private val GiB = 1024L * MiB

  test("computePartSize: размер части не превышает максимум из конфигурации") {
    assertEquals(Planner.computePartSize(mediaUsable = 10 * GiB, defaultMax = 2 * GiB, minimum = 64 * MiB), 2 * GiB)
  }

  test("computePartSize: размер части ограничен свободным местом носителя") {
    assertEquals(Planner.computePartSize(mediaUsable = 500 * MiB, defaultMax = 2 * GiB, minimum = 64 * MiB), 500 * MiB)
  }

  test("computePartSize: размер не ниже минимального") {
    assertEquals(Planner.computePartSize(mediaUsable = 10 * MiB, defaultMax = 2 * GiB, minimum = 64 * MiB), 64 * MiB)
  }

  test("partFileName: подставляет имя набора и номер с ведущими нулями") {
    assertEquals(Planner.partFileName("{setName}.part{number}", "vacation-archive", 1), "vacation-archive.part001")
    assertEquals(Planner.partFileName("{setName}.part{number}", "vacation-archive", 42), "vacation-archive.part042")
  }

  test("buildPlan: носители заполняются строго по порядку") {
    val media = List(mediaOf("E:\\", 3 * GiB), mediaOf("F:\\", 3 * GiB))
    val plan = build(4 * GiB, media, config(maxPart = 2 * GiB, minPart = 100 * MiB))

    assertEquals(plan.partSize, 2 * GiB)
    assertEquals(plan.parts.map(_.mediaNumber), List(1, 1, 2))
    assertEquals(plan.allocations.map(_.partNumbers), List(List(1, 2), List(3)))
  }

  test("buildPlan: части не выходят за пределы плана") {
    val media = List(mediaOf("E:\\", 3 * GiB), mediaOf("F:\\", 2 * GiB), mediaOf("G:\\", 5 * GiB))
    val plan = build(7 * GiB, media, config(maxPart = 1 * GiB, minPart = 100 * MiB))

    assertEquals(plan.parts.map(_.number), plan.parts.indices.map(_ + 1).toList)
    assertEquals(plan.parts.map(_.size).sum, 7 * GiB)
    assert(plan.parts.forall(_.size <= plan.partSize))
    assert(plan.parts.map(_.mediaNumber) == plan.parts.map(_.mediaNumber).sorted)
    assertEquals(plan.allocations.map(_.partNumbers).flatten, plan.parts.map(_.number))
    // ни один носитель не переполнен
    plan.allocations.zipWithIndex.foreach { (allocation, i) =>
      assert(allocation.writtenBytes + allocation.remainingBytes == Planner.capacityOf(media(i), i + 1, planConfig))
    }
  }

  test("buildPlan: не хватает суммарного объёма носителей") {
    val media = List(mediaOf("E:\\", 2 * GiB))
    val result = Planner.buildPlan(setOf(10 * GiB), media, planConfig)
    assert(result.isLeft)
    assert(result.swap.toOption.exists(e => e.isInstanceOf[AppError.NotEnoughTotalSpace]))
  }

  test("buildPlan: носитель не вмещает ни одной части") {
    val media = List(mediaOf("E:\\", 100 * MiB))
    val result = Planner.buildPlan(setOf(90 * MiB), media, config(maxPart = 1 * GiB, minPart = 1 * GiB))
    assert(result.isLeft)
    assert(result.swap.toOption.exists(e => e.isInstanceOf[AppError.NotEnoughSpaceOnMedia]))
  }

  test("buildPlan: носитель защищён от записи") {
    val media = List(mediaOf("E:\\", 5 * GiB).copy(writable = false))
    val result = Planner.buildPlan(setOf(1 * GiB), media, planConfig)
    assert(result.isLeft)
    assert(result.swap.toOption.exists(e => e.isInstanceOf[AppError.MediaNotWritable]))
  }

  test("buildPlan: размер части ограничен файловой системой") {
    val media = List(mediaOf("E:\\", 20 * GiB))
    val plan = build(6 * GiB, media, config(maxPart = 8 * GiB, minPart = 100 * MiB))
    assertEquals(plan.partSize, Planner.FileSizeLimit)
    assert(plan.warnings.exists(_.contains("файловой системы")))
  }

  test("buildPlan: предупреждение о свободном носителе") {
    val media = List(mediaOf("E:\\", 3 * GiB), mediaOf("F:\\", 3 * GiB))
    val plan = build(1 * GiB, media, config(maxPart = 1 * GiB, minPart = 100 * MiB))
    assertEquals(plan.allocations(1).partNumbers, Nil)
    assert(plan.warnings.exists(_.contains("Носитель 2")))
  }

  test("buildPlan: пустой набор не создаёт частей") {
    val plan = build(0L, List(mediaOf("E:\\", 3 * GiB)), planConfig, fileCount = 0)
    assertEquals(plan.parts, Nil)
    assert(plan.warnings.exists(_.contains("нет файлов")))
  }

  test("replanTail: проверенные части сохраняются, остаток пересчитывается заново") {
    // Первый носитель мал: часть набора попадает и на второй носитель.
    val media = List(mediaOf("E:\\", 350 * MiB), mediaOf("F:\\", 3 * GiB))
    val configE = config(maxPart = 100 * MiB, minPart = 50 * MiB)
    val original = build(500 * MiB, media, configE)
    assertEquals(original.parts.map(_.mediaNumber), List(1, 1, 1, 1, 2, 2))

    // Проверены все части первого носителя; остаток пересчитывается с уменьшенным размером части.
    val tailPlan = Planner
      .replanTail(setOf(500 * MiB), original, verifiedParts = List(1, 2, 3, 4), List(mediaOf("F:\\", 3 * GiB)), config(maxPart = 50 * MiB, minPart = 25 * MiB))
      .fold(err => fail(err.message), identity)

    // Проверенные части не изменяются.
    assertEquals(tailPlan.parts.take(4), original.parts.take(4))
    assertEquals(tailPlan.parts.take(4).map(_.mediaNumber), List(1, 1, 1, 1))
    // Оставшиеся ~151 МБ ложатся на второй носитель частями по 50 МБ: нумерация продолжается.
    assertEquals(tailPlan.parts.map(_.number), (1 to 8).toList)
    assertEquals(tailPlan.parts.drop(4).map(_.mediaNumber), List(2, 2, 2, 2))
    assertEquals(tailPlan.parts.map(_.size).sum, 500 * MiB)
    // Проверенный носитель в распределении сохраняется, остаток перестраивается с носителя 2.
    assertEquals(tailPlan.allocations.map(_.partNumbers), List(List(1, 2, 3, 4), List(5, 6, 7, 8)))
  }

  test("replanTail: не хватает места на оставшихся носителях") {
    val media = List(mediaOf("E:\\", 350 * MiB), mediaOf("F:\\", 3 * GiB))
    val original = build(500 * MiB, media, config(maxPart = 100 * MiB, minPart = 50 * MiB))
    val result = Planner.replanTail(
      setOf(500 * MiB),
      original,
      verifiedParts = List(1, 2, 3, 4),
      List(mediaOf("F:\\", 100 * MiB)),
      config(maxPart = 100 * MiB, minPart = 50 * MiB)
    )
    assert(result.isLeft)
    assert(result.swap.toOption.exists(_.isInstanceOf[AppError.NotEnoughTotalSpace]))
  }

  test("replanTail: без непроверенных частей план не меняется") {
    val media = List(mediaOf("E:\\", 350 * MiB), mediaOf("F:\\", 3 * GiB))
    val original = build(500 * MiB, media, config(maxPart = 100 * MiB, minPart = 50 * MiB))
    val verified = original.parts.map(_.number)
    val replanned = Planner
      .replanTail(setOf(500 * MiB), original, verifiedParts = verified, List(mediaOf("F:\\", 3 * GiB)), planConfig)
      .fold(err => fail(err.message), identity)
    assertEquals(replanned, original)
  }

  private val planConfig: transfer.config.Config = config(maxPart = 1 * GiB, minPart = 100 * MiB)

  private def build(
      totalSize: Long,
      media: List[Media],
      config: transfer.config.Config,
      fileCount: Int = 3
  ): Plan =
    Planner.buildPlan(setOf(totalSize, fileCount), media, config).fold(err => fail(err.message), identity)

  private def setOf(totalSize: Long, fileCount: Int = 3): TransferSet =
    TransferSet("8f31c2", "vacation-archive", "2026-10-14T18:30:00Z", Nil, totalSize, fileCount, 1, Nil, Nil)

  private def mediaOf(path: String, available: Long): Media =
    Media(label = path, path = path, totalSpace = available, availableSpace = available, writable = true, None)

  private def config(maxPart: Long, minPart: Long): transfer.config.Config =
    ConfigLoader.default.copy(
      defaultPartSize = maxPart,
      minimumPartSize = minPart,
      reservedSpacePerMedia = 0L
    )