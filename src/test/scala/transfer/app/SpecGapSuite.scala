package transfer.app

import transfer.TestFs.deleteTree
import transfer.cli.{Command, CommandParser}
import transfer.config.ConfigLoader
import transfer.domain.*

import java.nio.file.{Files, Path}

/** Проверка возможностей, добавленных по ТЗ: конфликты путей, подключение носителей, отчёт, манифест. */
class SpecGapSuite extends munit.FunSuite:

  // §2: конфликт относительных путей исключает лишний файл, решение обязательно до плана.

  test("конфликт относительных путей: лишний файл исключён, путь к плану заблокирован") {
    val root = Files.createTempDirectory("gap-conflict")
    // Два каталога с одинаковым именем: файлы внутри восстанавливаются под одним путём.
    val one = Files.createDirectories(root.resolve("x").resolve("data"))
    val two = Files.createDirectories(root.resolve("y").resolve("data"))
    val file1 = Files.writeString(one.resolve("report.txt"), "первый")
    val file2 = Files.writeString(two.resolve("report.txt"), "второй")
    val media = Files.createDirectory(root.resolve("m"))
    try
      val (s1, _) = run(App.empty(cfg, "transfer.json"), Command.CreateSet("demo"))
      val (s2, _) = run(s1, Command.AddDirectory(one.toString))
      val (s3, summary) = run(s2, Command.AddDirectory(two.toString))
      val set = s3.set.get
      assertEquals(set.fileCount, 1, "под одним именем восстанавливается только один файл")
      assertEquals(set.dropped, List(file2.toString))
      assertEquals(set.pathConflicts, List("data/report.txt"))
      assert(summary.contains("Исключено из-за конфликтов путей: 1"))

      // До решения конфликтов план не строится.
      val (s4, _) = run(s3, Command.AddMedia(media.toString))
      assert(App(Command.BuildPlan, s4).isLeft, "конфликт путей блокирует построение плана")

      val (s5, afterHint) = run(s4, Command.ShowSources)
      assert(afterHint.contains("конфликты относительных путей"), "пользователю предлагается действие")

      // Повторная проверка ничего не меняет: конфликт остаётся, пока пользователь не решит, что исключить.
      val (s6, rechecked) = run(s5, Command.RecheckSources)
      assert(rechecked.contains("Конфликтов путей: 1"))
      assert(App(Command.BuildPlan, s6).isLeft)

      val (s7, _) = run(s6, Command.RemoveSource(two.toString))
      assertEquals(s7.pendingConflicts, false)
      val (s8, planText) = run(s7, Command.BuildPlan)
      assert(s8.plan.isDefined)
      assert(planText.contains("Частей: 1"))
      assertEquals(s8.set.get.entries.filter(_.isFile).map(_.absPathOf), List(file1.toString))
    finally deleteTree(root)
  }

  test("remove-source снимает блокировку конфликтов") {
    val root = Files.createTempDirectory("gap-conflict-2")
    val one = Files.createDirectories(root.resolve("x").resolve("data"))
    val two = Files.createDirectories(root.resolve("y").resolve("data"))
    Files.writeString(one.resolve("doc.txt"), "a")
    Files.writeString(two.resolve("doc.txt"), "bb")
    val media = Files.createDirectory(root.resolve("m"))
    try
      val (s1, _) = run(App.empty(cfg, "transfer.json"), Command.CreateSet("demo"))
      val (s2, _) = run(s1, Command.AddDirectory(one.toString))
      val (s3, _) = run(s2, Command.AddDirectory(two.toString))
      val (s4, _) = run(s3, Command.AddMedia(media.toString))
      assert(App(Command.BuildPlan, s4).isLeft)
      val (s5, _) = run(s4, Command.RemoveSource(two.toString))
      assertEquals(s5.pendingConflicts, false)
      val (s6, _) = run(s5, Command.BuildPlan)
      assert(s6.plan.isDefined)
    finally deleteTree(root)
  }

  // §8: изменившиеся исходные файлы исключаются отдельной командой.

  test("exclude-changed исключает изменившиеся файлы и сбрасывает план") {
    val root = Files.createTempDirectory("gap-changed")
    val src = Files.createDirectories(root.resolve("src"))
    val stable = Files.writeString(src.resolve("stable.txt"), "одинаково")
    val changing = Files.writeString(src.resolve("changing.txt"), "было")
    val media = Files.createDirectory(root.resolve("m"))
    try
      val (s1, _) = run(App.empty(cfg, "transfer.json"), Command.CreateSet("demo"))
      val (s2, _) = run(s1, Command.AddDirectory(src.toString))
      val (s3, _) = run(s2, Command.AddMedia(media.toString))
      val (s4, _) = run(s3, Command.BuildPlan)
      assert(s4.plan.isDefined)
      assertEquals(s4.pendingUnreadable, false)

      val (s5, nothing) = run(s4, Command.ExcludeChanged)
      assert(nothing.contains("Изменившихся исходных файлов нет"), nothing)

      // Файл изменился после формирования набора.
      Thread.sleep(1100)
      Files.writeString(changing, "третий вариант")
      val (s6, message) = run(s5, Command.ExcludeChanged)
      assert(message.contains("Исключено изменившихся файлов: 1"), message)
      assertEquals(s6.set.get.fileCount, 1)
      assertEquals(s6.set.get.entries.filter(_.isFile).map(_.absPathOf), List(stable.toString))
      assertEquals(s6.plan, None, "план сбрасывается: распределение изменилось")
    finally deleteTree(root)
  }

  // §6, §9, §10: подключение носителя под другим путём и последовательный запрос.

  test("connect-media меняет путь носителя, новое имя сохраняется в манифесте") {
    val root = Files.createTempDirectory("gap-connect")
    val src = Files.createDirectory(root.resolve("src"))
    Files.writeString(src.resolve("data.txt"), "данные")
    val planned = Files.createDirectory(root.resolve("m1"))
    val actual = Files.createDirectory(root.resolve("m1-подключен"))
    val work = Files.createDirectory(root.resolve("work"))
    try
      val (s1, _) = run(App.empty(cfg(work), "transfer.json"), Command.CreateSet("demo"))
      val (s2, _) = run(s1, Command.AddDirectory(src.toString))
      val (s3, _) = run(s2, Command.AddMedia(planned.toString))
      val (s4, _) = run(s3, Command.BuildPlan)
      val (s5, _) = run(s4, Command.ConfirmWrite)

      // Пользователь подключает носитель по другому пути.
      val request: MediaRequestStub = (number, total, media) => Right(actual.toString)
      val result = App(Command.Write, s5, _ => true, Some(request))
      val (s6, message) = result.fold(err => fail(err.message), identity)
      assertEquals(s6.media.head.path, actual.toString, "фактический путь сохраняется")
      assertEquals(s6.manifest.get.mediaPaths, List(actual.toString))
      assertEquals(s6.manifest.get.mediaLabels, List("m1"))
      assert(message.contains("Итоговый отчёт"))
    finally deleteTree(root)
  }

  test("последовательный запрос носителей: носители запрашиваются по одному в порядке манифеста") {
    val root = Files.createTempDirectory("gap-request")
    val src = Files.createDirectory(root.resolve("src"))
    Files.writeString(src.resolve("a.txt"), "1")
    val m1 = Files.createDirectory(root.resolve("m1"))
    val m2 = Files.createDirectory(root.resolve("m2"))
    val work = Files.createDirectory(root.resolve("work"))
    try
      val (s1, _) = run(App.empty(cfg(work), "transfer.json"), Command.CreateSet("demo"))
      val (s2, _) = run(s1, Command.AddDirectory(src.toString))
      val (s3, _) = run(s2, Command.AddMedia(m1.toString))
      val (s4, _) = run(s3, Command.AddMedia(m2.toString))
      val (s5, _) = run(s4, Command.BuildPlan)
      val (s6, _) = run(s5, Command.ConfirmWrite)

      val asked = scala.collection.mutable.ListBuffer.empty[Int]
      val request: MediaRequestStub = (number, _, _) =>
        asked += number
        Right(if number == 1 then m1.toString else m2.toString)

      val (s7, _) = App(Command.Write, s6, _ => true, Some(request)).fold(err => fail(err.message), identity)
      assertEquals(asked.toList, List(1, 2), "носители запрашиваются последовательно")
      assertEquals(s7.manifest.get.completed, true)
    finally deleteTree(root)
  }

  test("label-media задаёт название, connect-media без пути — понятная ошибка") {
    val root = Files.createTempDirectory("gap-label")
    val m1 = Files.createDirectory(root.resolve("m1"))
    try
      val (s1, _) = run(App.empty(cfg, "transfer.json"), Command.AddMedia(m1.toString))
      val (s2, _) = run(s1, Command.LabelMedia(m1.toString, "Флешка 32 ГБ"))
      assertEquals(s2.media.head.label, "Флешка 32 ГБ")

      val unknown = App(Command.ConnectMedia(1, ""), s2)
      assert(unknown.isLeft)
      val wrongNumber = App(Command.ConnectMedia(7, m1.toString), s2)
      assert(wrongNumber.isLeft)
    finally deleteTree(root)
  }

  // §13: итоговый отчёт и журнал.

  test("report содержит состав, носители, части, манифест и признак завершённости") {
    val root = Files.createTempDirectory("gap-report")
    val src = Files.createDirectory(root.resolve("src"))
    Files.writeString(src.resolve("a.txt"), "данные для переноса")
    val m1 = Files.createDirectory(root.resolve("m1"))
    val work = Files.createDirectory(root.resolve("work"))
    try
      val (s1, _) = run(App.empty(cfg(work), "transfer.json"), Command.CreateSet("перенос"))
      val (s2, _) = run(s1, Command.AddDirectory(src.toString))
      val (s3, _) = run(s2, Command.AddMedia(m1.toString))
      val (s4, _) = run(s3, Command.BuildPlan)
      val (s5, _) = run(s4, Command.ConfirmWrite)
      val (s6, written) = run(s5, Command.Write)
      assert(written.contains("Итоговый отчёт"))

      val (s7, report) = run(s6, Command.ShowReport)
      assert(report.contains("Набор: перенос"))
      assert(report.contains("Исходные объекты: 1"))
      assert(report.contains("a.txt"))
      assert(report.contains("Носителей: 1"))
      assert(report.contains("Частей: 1"))
      assert(report.contains("Распределение частей"))
      assert(report.contains("части 1"))
      assert(report.contains("Путь к манифесту:"))
      assert(report.contains("набор завершён"))
      assertEquals(s7.errors, Nil)
    finally deleteTree(root)
  }

  test("после создания набора формируется итоговый отчёт (§13)") {
    val root = Files.createTempDirectory("gap-newset-report")
    try
      val (s1, created) = run(App.empty(cfg, "transfer.json"), Command.CreateSet("перенос"))
      assert(created.contains("Итоговый отчёт"))
      assert(created.contains("Набор: перенос"))
      assert(created.contains("Файлов: 0"))
      assert(created.contains("Носителей: 0"))
      assert(created.contains("Частей: 0"))
      assert(created.contains("план ещё не построен"))
      assert(created.contains("Путь к манифесту:"))
      assert(created.contains("набор не завершён"))
      assertEquals(s1.errors, Nil)

      // Отчёт доступен и по команде report.
      val (s2, report) = run(s1, Command.ShowReport)
      assert(report.contains("Итоговый отчёт"))
      assert(report.contains("Частей: 0"))
    finally deleteTree(root)
  }

  test("журнал ведёт запуск, создание набора, носители, план, запись и манифест") {
    val root = Files.createTempDirectory("gap-log")
    val src = Files.createDirectory(root.resolve("src"))
    Files.writeString(src.resolve("a.txt"), "журнал")
    val m1 = Files.createDirectory(root.resolve("m1"))
    val work = Files.createDirectory(root.resolve("work"))
    val logFile = work.resolve("app.log").toString
    val config = cfg(work).copy(logFile = logFile)
    try
      val (s1, _) = run(App.empty(config, "transfer.json"), Command.CreateSet("demo"))
      val (s2, _) = run(s1, Command.AddDirectory(src.toString))
      val (s3, _) = run(s2, Command.AddMedia(m1.toString))
      val (s4, _) = run(s3, Command.BuildPlan)
      val (s5, _) = run(s4, Command.ConfirmWrite)
      run(s5, Command.Write)

      val log = Files.readString(work.resolve("app.log"))
      assert(log.contains("Создан набор"), "создание набора")
      assert(log.contains("Добавлен носитель"), "добавление носителя")
      assert(log.contains("Построен план"), "построение плана")
      assert(log.contains("Начало записи набора"), "начало записи")
      assert(log.contains("Манифест"), "создание манифеста")
    finally deleteTree(root)
  }

  test("неудачное добавление источника не попадает в журнал как успешное") {
    val root = Files.createTempDirectory("gap-log-fail")
    val src = Files.createDirectory(root.resolve("src"))
    val file = Files.writeString(src.resolve("a.txt"), "данные")
    val work = Files.createDirectory(root.resolve("work"))
    val logPath = work.resolve("app.log")
    val config = cfg(work).copy(logFile = logPath.toString)
    def logged(): String = if Files.exists(logPath) then Files.readString(logPath) else ""
    def loggedAdded(kind: String, path: String): Boolean =
      logged().linesIterator.exists(_.endsWith(s"$kind: $path"))
    try
      val start = App.empty(config, "transfer.json")

      // Успешное добавление фиксируется в журнале.
      val (ok, _) = run(start, Command.AddFile(file.toString))
      assert(ok.selected.nonEmpty)
      assert(loggedAdded("Добавлен исходный файл", file.toString), "успешное добавление попадает в журнал")

      // Отсутствующий файл: ошибка есть, ложной записи об успехе быть не должно.
      val missing = root.resolve("нет-такого.txt").toString
      assert(App(Command.AddFile(missing), start).isLeft, "нет файла — ошибка")
      assert(!loggedAdded("Добавлен исходный файл", missing), "несуществующий файл не записан как добавленный")

      // Отсутствующий каталог — то же самое для add-directory.
      val missingDir = root.resolve("нет-такого-каталога").toString
      assert(App(Command.AddDirectory(missingDir), start).isLeft, "нет каталога — ошибка")
      assert(!loggedAdded("Добавлен исходный каталог", missingDir), "несуществующий каталог не записан как добавленный")

      // Существующий каталог в add-file: ошибка есть, ложной записи тоже нет.
      val dirAsFile = src.toString
      assert(App(Command.AddFile(dirAsFile), start).isLeft, "каталог вместо файла — ошибка")
      assert(!loggedAdded("Добавлен исходный файл", dirAsFile), "каталог вместо файла не записан как добавленный")
    finally deleteTree(root)
  }

  test("файл внутри добавленного каталога не учитывается дважды") {
    val root = Files.createTempDirectory("gap-dup")
    val src = Files.createDirectory(root.resolve("papka"))
    val file = Files.writeString(src.resolve("01.txt"), "данные")
    val size = Files.size(file)
    try
      val start = App.empty(cfg, "transfer.json")
      val (created, _) = run(start, Command.CreateSet("demo"))

      // Сначала каталог, затем лежащий в нём файл.
      val (a, summaryA) = run(created, Command.AddDirectory(src.toString))
      val (b, summaryB) = run(a, Command.AddFile(file.toString))
      val setB = b.set.get
      assertEquals(setB.fileCount, 1, "файл не посчитан дважды")
      assertEquals(setB.totalSize, size, "общий объём не удвоен")
      assertEquals(setB.pathConflicts, Nil)
      assertEquals(setB.entries.count(e => e.absPathOf == file.toString), 1, "объект в наборе один")
      assert(summaryB.contains("Файлов: 1"), "в сводке один файл")
      assert(summaryB.contains("Вложенных каталогов: 0"), "добавленный каталог не считается вложенным")

      // Обратный порядок даёт тот же результат.
      val (c, _) = run(created, Command.AddFile(file.toString))
      val (d, summaryD) = run(c, Command.AddDirectory(src.toString))
      assertEquals(d.set.get.fileCount, 1, "порядок команд не влияет на счётчик")
      assertEquals(d.set.get.totalSize, size)
      assertEquals(d.set.get.entries.count(e => e.absPathOf == file.toString), 1)
      assert(summaryD.contains("Файлов: 1"))
      assert(!summaryA.contains("Файлов: 2"))
    finally deleteTree(root)
  }

  // §7: манифест переносимого набора.

  test("манифест сохраняет состав набора и читается обратно") {
    val manifest = ManifestIOSample()
    val json = transfer.operations.ManifestIO.manifestToJson(manifest)
    val back = transfer.operations.ManifestIO.jsonToManifest(json).fold(err => fail(err.message), identity)
    assertEquals(back.setId, manifest.setId)
    assertEquals(back.name, manifest.name)
    assertEquals(back.files, manifest.files)
    assertEquals(back.parts, manifest.parts)
    assertEquals(back.mediaCount, manifest.mediaCount)
    assert(back.completed)
    assert(!json.obj.contains("archive"), "в манифесте нет сведений об архивации")
  }

  // §1, §14: разбор команд без исключений.

  test("некорректные команды разбираются в ошибку, а не в исключение") {
    assert(CommandParser.parse("move-media раз два").isLeft)
    assert(CommandParser.parse("connect-media раз").isLeft)
    assert(CommandParser.parse("move-media 1").isLeft)
    assert(CommandParser.parse("label-media только-путь").isLeft)
    assert(CommandParser.parse("limit-media C:\\диск 1").isLeft)
    assertEquals(CommandParser.parse("move-media 1 2"), Right(Command.MoveMedia(1, 2)))
    assertEquals(CommandParser.parse("connect-media 2"), Right(Command.ConnectMedia(2, "")))
    assertEquals(
      CommandParser.parse("connect-media 2 D:\\носитель 2"),
      Right(Command.ConnectMedia(2, "D:\\носитель 2"))
    )
    assertEquals(CommandParser.parse("label-media D:\\м R2"), Right(Command.LabelMedia("D:\\м", "R2")))
    assertEquals(CommandParser.parse("report"), Right(Command.ShowReport))
    assertEquals(CommandParser.parse("exclude-changed"), Right(Command.ExcludeChanged))
  }

  // §3: ручное ограничение носителя — окончательный разрешённый объём.

  test("limit-media ограничивает носитель независимо от свободного места") {
    val root = Files.createTempDirectory("gap-limit")
    val m1 = Files.createDirectory(root.resolve("m1"))
    try
      val (s1, _) = run(App.empty(cfg, "transfer.json"), Command.AddMedia(m1.toString))
      val (s2, message) = run(s1, Command.LimitMedia(m1.toString, 5L))
      assert(message.contains("5 Б"))
      assertEquals(s2.media.head.manualLimit, Some(5L))
      assertEquals(Media.usableSpace(s2.media.head, 0L), 5L)
    finally deleteTree(root)
  }

  private type MediaRequestStub = transfer.operations.MediaRequest

  private def ManifestIOSample(): Manifest =
    Manifest(
      setId = "abc123",
      name = "набор",
      createdAt = "2026-01-01T00:00:00Z",
      fileCount = 1,
      totalSize = 100L,
      files = List(ManifestFile("src/документ.txt", 100L, "abc")),
      dirs = List("src"),
      parts = Nil,
      mediaLabels = List("m1", "m2"),
      mediaPaths = List("D:\\m1", "E:\\m2"),
      completed = true,
      checksumAlgorithm = "SHA-256",
      preserveEmptyDirectories = true,
      fileNameTemplate = "{setName}.part{number}"
    )

  private def run(state: AppState, cmd: Command): (AppState, String) =
    App(cmd, state).fold(err => fail(s"$cmd: ${err.message}"), identity)

  private def cfg(work: Path): transfer.config.Config =
    ConfigLoader.default.copy(
      defaultPartSize = 64L,
      minimumPartSize = 16L,
      reservedSpacePerMedia = 0L,
      workDirectory = work.toString
    )

  private def cfg: transfer.config.Config = cfg(Files.createTempDirectory("gap-cfg"))
