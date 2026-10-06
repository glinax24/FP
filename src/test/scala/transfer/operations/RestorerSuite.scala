package transfer.operations

import transfer.TestFs
import transfer.config.ConfigLoader
import transfer.domain.*
import transfer.planning.Planner

import java.nio.file.{Files, Path}

/** Проверка восстановления исходных файлов по манифесту (§10). */
class RestorerSuite extends munit.FunSuite:

  private val KiB = 1024L
  private val MiB = 1024L * KiB
  private val Media1Space = 4096L + Planner.ManifestOverhead + Planner.MediaDescriptorOverhead
  private val Media2Space = 8L * MiB
  private val PartSize = 4096L
  private val SetId = "8f31c2"

  private case class Env(src: Path, media1: Path, media2: Path, work: Path, out: Path, config: transfer.config.Config, set: TransferSet)

  private def withEnv[A](body: Env => A): A =
    val root = Files.createTempDirectory("transfer-restore")
    try
      val src = Files.createDirectories(root.resolve("src"))
      val media1 = Files.createDirectories(root.resolve("media1"))
      val media2 = Files.createDirectories(root.resolve("media2"))
      val work = Files.createDirectories(root.resolve("work"))
      val out = root.resolve("restored")
      val config = ConfigLoader.default.copy(
        workDirectory = work.toString,
        logFile = work.resolve("log.txt").toString,
        defaultPartSize = PartSize,
        minimumPartSize = KiB,
        reservedSpacePerMedia = 0L
      )
      TestFs.createFile(src.resolve("a.txt"), TestFs.pattern(3000))
      TestFs.createFile(src.resolve("b.bin"), TestFs.pattern(5000))
      TestFs.createFile(src.resolve("empty.txt"), Array.emptyByteArray)
      Files.createDirectories(src.resolve("empty-dir"))
      TestFs.createFile(src.resolve("sub/c.txt"), TestFs.pattern(2000))
      val set = Collector.collect(SetId, "vacation-archive", List(SelectedItem.Directory(src.toString, "src")))
        .fold(err => fail(err.message), identity)
      body(Env(src, media1, media2, work, out, config, set))
    finally TestFs.deleteTree(root)

  private def mediaList(env: Env): List[Media] =
    Media("BLUE-USB", env.media1.toString, Media1Space, Media1Space, writable = true, None) ::
      Media("OLD-FLASH", env.media2.toString, Media2Space, Media2Space, writable = true, None) :: Nil

  /** Записывает набор и возвращает готовый к восстановлению манифест. */
  private def written(env: Env): Manifest =
    val plan = Planner.buildPlan(env.set, mediaList(env), env.config).fold(err => fail(err.message), identity)
    Writer.writeSet(env.set, plan, mediaList(env), env.config).fold(err => fail(err.message), identity).manifest

  private def restore(env: Env, config: transfer.config.Config = null, confirm: String => Boolean = _ => false): Either[AppError, RestoreReport] =
    Restorer.restore(written(env), List(env.media1, env.media2), env.out, Option(config).getOrElse(env.config), confirm)

  private def content(env: Env, relPath: String): Array[Byte] = TestFs.readAll(env.out.resolve(relPath))

  test("restore: файлы, пути и содержимое восстанавливаются полностью") {
    withEnv { env =>
      val manifest = written(env)
      val report = Restorer.restore(manifest, List(env.media1, env.media2), env.out, env.config).fold(err => fail(err.message), identity)

      assertEquals(report.restoredFiles, 4)
      assertEquals(TestFs.readAll(env.out.resolve("src/a.txt")).toList, TestFs.pattern(3000).toList)
      assertEquals(TestFs.readAll(env.out.resolve("src/b.bin")).toList, TestFs.pattern(5000).toList)
      assertEquals(TestFs.readAll(env.out.resolve("src/empty.txt")).toList, Nil)
      assertEquals(TestFs.readAll(env.out.resolve("src/sub/c.txt")).toList, TestFs.pattern(2000).toList)
      assertEquals(report.problems.map(p => p.relPath -> p.reason), Nil)
      assertEquals(report.verifiedFiles, 4)
      assertEquals(report.failedFiles, 0)
      assertEquals(report.status, "успешно")
      assertEquals(report.mediaCount, 2)
      assertEquals(report.partCount, 3)
      assertEquals(report.restoredBytes, 10000L)
      assertEquals(content(env, "src/a.txt").toList, TestFs.pattern(3000).toList)
      assertEquals(content(env, "src/b.bin").toList, TestFs.pattern(5000).toList)
      assertEquals(content(env, "src/sub/c.txt").toList, TestFs.pattern(2000).toList)
    }
  }

  test("restore: файл нулевой длины создаётся") {
    withEnv { env =>
      restore(env).fold(err => fail(err.message), identity)

      assert(Files.exists(env.out.resolve("src/empty.txt")))
      assertEquals(Files.size(env.out.resolve("src/empty.txt")), 0L)
    }
  }

  test("restore: содержимое файлов не переставляется, пустой файл создаётся в новом каталоге") {
    val root = Files.createTempDirectory("transfer-restore-order")
    try
      val src = Files.createDirectories(root.resolve("src"))
      val work = Files.createDirectories(root.resolve("work"))
      val media1 = Files.createDirectories(root.resolve("media1"))
      val out = root.resolve("restored")
      TestFs.createFile(src.resolve("alpha.txt"), TestFs.pattern(700))
      TestFs.createFile(src.resolve("babl.txt"), TestFs.pattern(900))
      TestFs.createFile(src.resolve("empty-dir/zero.txt"), Array.emptyByteArray)
      val config = ConfigLoader.default.copy(
        workDirectory = work.toString,
        logFile = work.resolve("log.txt").toString,
        defaultPartSize = PartSize,
        minimumPartSize = KiB,
        reservedSpacePerMedia = 0L
      )
      // Порядок добавления обратный алфавитному: babl раньше alpha.
      val items = List(
        SelectedItem.File(src.resolve("babl.txt").toString, "babl.txt"),
        SelectedItem.File(src.resolve("alpha.txt").toString, "alpha.txt"),
        SelectedItem.File(src.resolve("empty-dir/zero.txt").toString, "empty-dir/zero.txt")
      )
      val set = Collector.collect(SetId, "order", items).fold(err => fail(err.message), identity)
      val media = List(Media("BLUE-USB", media1.toString, Media1Space, Media1Space, writable = true, None))
      val plan = Planner.buildPlan(set, media, config).fold(err => fail(err.message), identity)
      val manifest = Writer.writeSet(set, plan, media, config).fold(err => fail(err.message), identity).manifest

      // Манифест описывает файлы в том же порядке, в котором они лежат внутри части.
      assertEquals(manifest.files.map(_.relPath), List("alpha.txt", "babl.txt", "empty-dir/zero.txt"))

      val report = Restorer.restore(manifest, List(media1), out, config).fold(err => fail(err.message), identity)

      assertEquals(report.problems.map(p => p.relPath -> p.reason), Nil, "ошибок восстановления нет")
      assertEquals(TestFs.readAll(out.resolve("alpha.txt")).toList, TestFs.pattern(700).toList, "alpha.txt не переставлен")
      assertEquals(TestFs.readAll(out.resolve("babl.txt")).toList, TestFs.pattern(900).toList, "babl.txt не переставлен")
      assert(Files.exists(out.resolve("empty-dir/zero.txt")), "пустой файл создан")
      assertEquals(TestFs.readAll(out.resolve("empty-dir/zero.txt")).toList, Nil)
      assertEquals(report.restoredFiles, 3)
      assertEquals(report.verifiedFiles, 3)
      assertEquals(report.failedFiles, 0)
      assertEquals(report.status, "успешно")
    finally TestFs.deleteTree(root)
  }

  test("restore: пустые каталоги восстанавливаются") {
    withEnv { env =>
      restore(env).fold(err => fail(err.message), identity)

      assert(Files.isDirectory(env.out.resolve("src/empty-dir")))
    }
  }

  test("restore: пустые каталоги не создаются, если это запрещено конфигурацией") {
    withEnv { env =>
      // Настройка фиксируется в манифесте при записи (§7) и применяется при восстановлении.
      val noDirs = env.config.copy(preserveEmptyDirectories = false)
      val plan = Planner.buildPlan(env.set, mediaList(env), noDirs).fold(err => fail(err.message), identity)
      val manifest = Writer.writeSet(env.set, plan, mediaList(env), noDirs).fold(err => fail(err.message), identity).manifest
      val report = Restorer.restore(manifest, List(env.media1, env.media2), env.out, env.config).fold(err => fail(err.message), identity)

      assertEquals(report.failedFiles, 0)
      assert(!Files.isDirectory(env.out.resolve("src/empty-dir")))
    }
  }

  test("restore: части на носителях остаются на месте") {
    withEnv { env =>
      val manifest = written(env)
      Restorer.restore(manifest, List(env.media1, env.media2), env.out, env.config).fold(err => fail(err.message), identity)

      manifest.parts.foreach { part =>
        val root = if part.mediaNumber == 1 then env.media1 else env.media2
        assert(Files.exists(root.resolve(part.fileName)))
      }
    }
  }

  test("restore: незавершённый набор не восстанавливается") {
    withEnv { env =>
      val manifest = written(env).copy(completed = false)

      val result = Restorer.restore(manifest, List(env.media1, env.media2), env.out, env.config)

      assertEquals(result, Left(AppError.SetNotCompleted("vacation-archive")))
    }
  }

  test("restore: отключённый носитель останавливает восстановление") {
    withEnv { env =>
      val manifest = written(env)

      val result = Restorer.restore(manifest, List(env.media1), env.out, env.config)

      assert(result.isLeft)
      assert(result.swap.toOption.exists(_.isInstanceOf[AppError.MediaUnavailable]))
    }
  }

  test("restore: испорченная часть обнаруживается до использования") {
    withEnv { env =>
      val manifest = written(env)
      TestFs.createFile(env.media2.resolve("vacation-archive.part003"), TestFs.pattern(1808))

      val result = Restorer.restore(manifest, List(env.media1, env.media2), env.out, env.config)

      assert(result.isLeft)
      assert(result.swap.toOption.exists(_.isInstanceOf[AppError.ChecksumMismatch]))
    }
  }

  test("restore: режим запрета перезаписи оставляет существующий файл нетронутым") {
    withEnv { env =>
      val config = env.config.copy(overwriteMode = transfer.config.OverwriteMode.Forbid)
      val existing = TestFs.createFile(env.out.getParent.resolve("restored-src-placeholder"), Array.emptyByteArray)
      Files.deleteIfExists(existing)
      Files.createDirectories(env.out.resolve("src"))
      TestFs.createFile(env.out.resolve("src/a.txt"), "старое содержимое".getBytes("UTF-8"))

      val report = restore(env, config).fold(err => fail(err.message), identity)

      assertEquals(report.restoredFiles, 3)
      assertEquals(report.failedFiles, 1)
      assertEquals(report.problems.map(_.relPath), List("src/a.txt"))
      assertEquals(String(new String(content(env, "src/a.txt"), "UTF-8")), "старое содержимое")
      // остальные файлы восстановлены полностью
      assertEquals(content(env, "src/b.bin").toList, TestFs.pattern(5000).toList)
    }
  }

  test("restore: режим переименования создаёт файл с новым именем") {
    withEnv { env =>
      val config = env.config.copy(overwriteMode = transfer.config.OverwriteMode.Rename)
      Files.createDirectories(env.out.resolve("src"))
      TestFs.createFile(env.out.resolve("src/a.txt"), "старое содержимое".getBytes("UTF-8"))

      val report = restore(env, config).fold(err => fail(err.message), identity)

      assertEquals(report.failedFiles, 0)
      assertEquals(String(new String(content(env, "src/a.txt"), "UTF-8")), "старое содержимое")
      assertEquals(content(env, "src/a (1).txt").toList, TestFs.pattern(3000).toList)
    }
  }

  test("restore: режим перезаписи заменяет существующий файл") {
    withEnv { env =>
      val config = env.config.copy(overwriteMode = transfer.config.OverwriteMode.Overwrite)
      Files.createDirectories(env.out.resolve("src"))
      TestFs.createFile(env.out.resolve("src/a.txt"), "старое содержимое".getBytes("UTF-8"))

      val report = restore(env, config).fold(err => fail(err.message), identity)

      assertEquals(report.failedFiles, 0)
      assertEquals(report.verifiedFiles, 4)
      assertEquals(content(env, "src/a.txt").toList, TestFs.pattern(3000).toList)
    }
  }

  test("restore: режим запроса подтверждения без согласия не перезаписывает файл") {
    withEnv { env =>
      Files.createDirectories(env.out.resolve("src"))
      TestFs.createFile(env.out.resolve("src/a.txt"), "старое содержимое".getBytes("UTF-8"))

      val report = restore(env, env.config.copy(overwriteMode = transfer.config.OverwriteMode.Ask), _ => false)
        .fold(err => fail(err.message), identity)

      assertEquals(report.failedFiles, 1)
      assertEquals(String(new String(content(env, "src/a.txt"), "UTF-8")), "старое содержимое")
    }
  }

  test("restore: режим запроса подтверждения с согласием перезаписывает файл") {
    withEnv { env =>
      Files.createDirectories(env.out.resolve("src"))
      TestFs.createFile(env.out.resolve("src/a.txt"), "старое содержимое".getBytes("UTF-8"))

      val report = restore(env, env.config.copy(overwriteMode = transfer.config.OverwriteMode.Ask), _ => true)
        .fold(err => fail(err.message), identity)

      assertEquals(report.failedFiles, 0)
      assertEquals(content(env, "src/a.txt").toList, TestFs.pattern(3000).toList)
    }
  }

  test("restore: подделанный относительный путь не выходит за пределы каталога") {
    withEnv { env =>
      val manifest = written(env)
      val evil = manifest.copy(files = manifest.files.map(f => f.copy(relPath = "../" + f.relPath)))
      // Даже с разрешённой перезаписью путь за пределы каталога восстановления не используется.
      val config = env.config.copy(overwriteMode = transfer.config.OverwriteMode.Overwrite)

      val report = Restorer.restore(evil, List(env.media1, env.media2), env.out, config, _ => true)
        .fold(err => fail(err.message), identity)

      assertEquals(report.restoredFiles, 0)
      assertEquals(report.failedFiles, 4)
      // исходные файлы не тронуты
      assertEquals(TestFs.readAll(env.src.resolve("a.txt")).toList, TestFs.pattern(3000).toList)
      assertEquals(TestFs.readAll(env.src.resolve("b.bin")).toList, TestFs.pattern(5000).toList)
    }
  }

  test("restore: отчёт содержит сведения о наборе и каталоге") {
    withEnv { env =>
      val report = restore(env).fold(err => fail(err.message), identity)
      val text = transfer.app.Report.restoreSummary(report)

      assert(text.contains("Набор: vacation-archive"))
      assert(text.contains("Восстановлено файлов: 4"))
      assert(text.contains("Итоговый статус: успешно"))
    }
  }
