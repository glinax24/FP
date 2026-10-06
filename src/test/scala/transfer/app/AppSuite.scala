package transfer.app

import transfer.TestFs.deleteTree
import transfer.cli.Command
import transfer.config.ConfigLoader
import transfer.domain.AppError

import java.nio.file.{Files, Path}

/** Проверка переходов состояния приложения по командам CLI. */
class AppSuite extends munit.FunSuite:

  private val MiB = 1024L * 1024

  test("сценарий: create-set → add-file → add-media → plan") {
    val root = Files.createTempDirectory("app-test")
    val file = Files.writeString(root.resolve("a.txt"), "hello")
    val mediaDir = Files.createDirectory(root.resolve("m1"))
    try
      var state = App.empty(testConfig, "transfer.json")

      val (afterSet, _) = run(state, Command.CreateSet("demo"))
      assertEquals(afterSet.set.map(_.name), Some("demo"))
      assert(afterSet.set.exists(_.id.nonEmpty))

      val (afterAdd, summary) = run(afterSet, Command.AddFile(file.toString))
      assert(summary.contains("Файлов: 1"))
      assertEquals(afterAdd.set.map(_.totalSize), Some(5L))

      val (afterMedia, _) = run(afterAdd, Command.AddMedia(mediaDir.toString))
      assertEquals(afterMedia.media.size, 1)

      val (afterPlan, planText) = run(afterMedia, Command.BuildPlan)
      assertEquals(afterPlan.plan.map(_.parts.size), Some(1))
      assertEquals(afterPlan.plan.map(_.parts.map(_.fileName)), Some(List("demo.part001")))
      assert(planText.contains("Частей: 1"))
    finally deleteTree(root)
  }

  test("sources до create-set — ошибка") {
    val state = App.empty(testConfig, "transfer.json")
    assert(App(Command.ShowSources, state).isLeft)
  }

  test("plan без носителей — ошибка") {
    val root = Files.createTempDirectory("app-test-2")
    val file = Files.writeString(root.resolve("a.txt"), "hello")
    try
      var state = App.empty(testConfig, "transfer.json")
      val (afterSet, _) = run(state, Command.CreateSet("demo"))
      val (afterAdd, _) = run(afterSet, Command.AddFile(file.toString))
      val result = App(Command.BuildPlan, afterAdd)
      assert(result.swap.toOption.exists(e => e.isInstanceOf[AppError.MissingState]))
    finally deleteTree(root)
  }

  test("add-media на несуществующий каталог — ошибка") {
    val state = App.empty(testConfig, "transfer.json")
    val result = App(Command.AddMedia("Z:\\нет-такого-носителя"), state)
    assert(result.swap.toOption.exists(e => e.isInstanceOf[AppError.MediaUnavailable]))
  }

  test("write без плана — ошибка") {
    val state = App.empty(testConfig, "transfer.json")
    val (afterSet, _) = run(state, Command.CreateSet("demo"))
    assert(App(Command.Write, afterSet).isLeft)
  }

  test("write требует подтверждения плана; write-confirm снимает блокировку") {
    val root = Files.createTempDirectory("app-test-write")
    val file = Files.writeString(root.resolve("a.txt"), "hello")
    val mediaDir = Files.createDirectory(root.resolve("m1"))
    val cfg = testConfig.copy(workDirectory = root.resolve("work").toString)
    try
      var state = App.empty(cfg, "transfer.json")
      val (afterSet, _) = run(state, Command.CreateSet("demo"))
      val (afterAdd, _) = run(afterSet, Command.AddFile(file.toString))
      val (afterMedia, _) = run(afterAdd, Command.AddMedia(mediaDir.toString))
      val (afterPlan, _) = run(afterMedia, Command.BuildPlan)

      assert(App(Command.Write, afterPlan).isLeft)

      val (confirmed, _) = run(afterPlan, Command.ConfirmWrite)
      assert(confirmed.planConfirmed)

      val (afterWrite, summary) = run(confirmed, Command.Write)
      assert(summary.contains("записан"))
      assert(afterWrite.manifest.nonEmpty)
      assert(!afterWrite.planConfirmed)
    finally deleteTree(root)
  }

  test("remove-source исключает источник из набора") {
    val root = Files.createTempDirectory("app-test-rm")
    val f1 = Files.writeString(root.resolve("a.txt"), "aaa")
    val f2 = Files.writeString(root.resolve("b.txt"), "bbbb")
    try
      var state = App.empty(testConfig, "transfer.json")
      val (afterSet, _) = run(state, Command.CreateSet("demo"))
      val (afterAdd, _) = run(afterSet, Command.AddFile(f1.toString))
      val (afterAdd2, _) = run(afterAdd, Command.AddFile(f2.toString))
      assertEquals(afterAdd2.set.map(_.fileCount), Some(2))

      val (afterRemove, _) = run(afterAdd2, Command.RemoveSource(f1.toString))
      assertEquals(afterRemove.set.map(_.fileCount), Some(1))
      assertEquals(afterRemove.set.map(_.totalSize), Some(4L))
      assert(App(Command.RemoveSource("Z:\\нет-такого-файла"), afterRemove).isLeft)
    finally deleteTree(root)
  }

  test("remove-media и move-media корректируют список носителей") {
    val root = Files.createTempDirectory("app-test-media")
    val dir1 = Files.createDirectory(root.resolve("one"))
    val dir2 = Files.createDirectory(root.resolve("two"))
    try
      var state = App.empty(testConfig, "transfer.json")
      val (afterMedia, _) = run(state, Command.AddMedia(dir1.toString))
      val (afterMedia2, _) = run(afterMedia, Command.AddMedia(dir2.toString))
      assertEquals(afterMedia2.media.map(_.path), List(dir1.toString, dir2.toString))

      val (afterMove, _) = run(afterMedia2, Command.MoveMedia(1, 2))
      assertEquals(afterMove.media.map(_.path), List(dir2.toString, dir1.toString))
      assert(afterMove.plan.isEmpty)

      val (afterRemove, _) = run(afterMove, Command.RemoveMedia(dir2.toString))
      assertEquals(afterRemove.media.map(_.path), List(dir1.toString))
      assert(App(Command.RemoveMedia("Z:\\нет-такого"), afterRemove).isLeft)
    finally deleteTree(root)
  }

  test("limit-media уменьшает доступный объём носителя") {
    val root = Files.createTempDirectory("app-test-limit")
    val dir1 = Files.createDirectory(root.resolve("one"))
    try
      var state = App.empty(testConfig, "transfer.json")
      val (afterMedia, _) = run(state, Command.AddMedia(dir1.toString))
      val (afterLimit, message) = run(afterMedia, Command.LimitMedia(dir1.toString, 1024L))
      assertEquals(afterLimit.media.head.manualLimit, Some(1024L))
      assert(message.contains("разрешено использовать"))
      assert(App(Command.LimitMedia("Z:\\нет-такого", 100L), afterMedia).isLeft)
    finally deleteTree(root)
  }

  test("rescan-media обновляет сведения о носителях") {
    val root = Files.createTempDirectory("app-test-rescan")
    val dir1 = Files.createDirectory(root.resolve("one"))
    try
      var state = App.empty(testConfig, "transfer.json")
      val (afterMedia, _) = run(state, Command.AddMedia(dir1.toString))
      val (afterRescan, message) = run(afterMedia, Command.RescanMedia)
      assertEquals(afterRescan.media.size, 1)
      assert(message.contains("Носители"))
    finally deleteTree(root)
  }

  test("exclude-unreadable без нечитаемых файлов — снимает флаг") {
    val root = Files.createTempDirectory("app-test-unreadable")
    val file = Files.writeString(root.resolve("a.txt"), "hello")
    try
      var state = App.empty(testConfig, "transfer.json")
      val (afterSet, _) = run(state, Command.CreateSet("demo"))
      val (afterAdd, _) = run(afterSet, Command.AddFile(file.toString))
      val (afterExcl, message) = run(afterAdd, Command.ExcludeUnreadable)
      assert(!afterExcl.pendingUnreadable)
      assert(message.contains("Нечитаемых файлов в наборе нет"))
    finally deleteTree(root)
  }

  test("recheck-sources повторно собирает набор") {
    val root = Files.createTempDirectory("app-test-recheck")
    val file = Files.writeString(root.resolve("a.txt"), "hello")
    try
      var state = App.empty(testConfig, "transfer.json")
      val (afterSet, _) = run(state, Command.CreateSet("demo"))
      val (afterAdd, _) = run(afterSet, Command.AddFile(file.toString))
      val (afterRecheck, message) = run(afterAdd, Command.RecheckSources)
      assert(message.contains("Файлов"))
      assert(!afterRecheck.pendingUnreadable)
    finally deleteTree(root)
  }

  test("rebuild очищает набор и сохраняет имя") {
    val root = Files.createTempDirectory("app-test-rebuild")
    val file = Files.writeString(root.resolve("a.txt"), "hello")
    try
      var state = App.empty(testConfig, "transfer.json")
      val (afterSet, _) = run(state, Command.CreateSet("demo"))
      val (afterAdd, _) = run(afterSet, Command.AddFile(file.toString))
      val (afterRebuild, _) = run(afterAdd, Command.Rebuild)
      assertEquals(afterRebuild.set.map(_.name), Some("demo"))
      assertEquals(afterRebuild.set.map(_.fileCount), Some(0))
    finally deleteTree(root)
  }

  test("open-manifest: загрузка манифеста из файла и ошибка для отсутствующего файла") {
    val root = Files.createTempDirectory("app-test-manifest")
    val mediaDir = Files.createDirectory(root.resolve("m1"))
    try
      var state = App.empty(testConfig, "transfer.json")
      val (afterSet, _) = run(state, Command.CreateSet("demo"))
      val (afterMedia, _) = run(afterSet, Command.AddMedia(mediaDir.toString))
      val manifest = transfer.domain.Manifest(
        setId = "123e4567-e89b-12d3-a456-426614174000",
        name = "demo",
        createdAt = "2026-10-14T18:30:00Z",
        fileCount = 1,
        totalSize = 5L,
        files = List(transfer.domain.ManifestFile("a.txt", 5L, "deadbeef")),
        dirs = Nil,
        parts = List(transfer.domain.Part(1, "demo.part001", 1, 5L, None)),
        mediaLabels = List(transfer.util.Fs.baseName(mediaDir.toString)),
        mediaPaths = List(mediaDir.toString),
        completed = true
      )
      transfer.operations.ManifestIO.writeManifest(manifest, root, "manifest.json")
      val manifestPath = root.resolve("manifest.json")
      val (afterOpen, message) = run(afterMedia, Command.OpenManifest(manifestPath.toString))
      assert(afterOpen.manifest.nonEmpty)
      assert(message.contains("Манифест загружен"))
      assert(App(Command.OpenManifest(root.resolve("нет.json").toString), afterMedia).isLeft)
    finally deleteTree(root)
  }

  test("verify и restore без манифеста — ошибка") {
    val state = App.empty(testConfig, "transfer.json")
    assert(App(Command.Verify, state).isLeft)
    assert(App(Command.Restore("D:\\Restore"), state).isLeft)
  }

  test("move-media с неверными позициями — ошибка") {
    val root = Files.createTempDirectory("app-test-move")
    val dir1 = Files.createDirectory(root.resolve("one"))
    try
      var state = App.empty(testConfig, "transfer.json")
      val (afterMedia, _) = run(state, Command.AddMedia(dir1.toString))
      assert(App(Command.MoveMedia(0, 2), afterMedia).isLeft)
      assert(App(Command.MoveMedia(1, 5), afterMedia).isLeft)
    finally deleteTree(root)
  }

  private def run(state: AppState, cmd: Command): (AppState, String) =
    App(cmd, state).fold(err => fail(err.message), identity)

  private def testConfig: transfer.config.Config =
    ConfigLoader.default.copy(
      defaultPartSize = 64L,
      minimumPartSize = 16L,
      reservedSpacePerMedia = 0L
    )
