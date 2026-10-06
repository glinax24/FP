package transfer.operations

import transfer.TestFs
import transfer.config.ConfigLoader
import transfer.domain.*
import transfer.planning.Planner
import transfer.util.Checksum

import java.nio.file.{Files, Path}

/** Проверка записи частей на носители, описаний носителей, манифеста и продолжения записи (§5–§9). */
class WriterSuite extends munit.FunSuite:

  private val KiB = 1024L
  private val MiB = 1024L * KiB

  /** Первый носитель вмещает ровно одну часть: 4096 байт данных плюс служебные файлы. */
  private val Media1Space = 4096L + Planner.ManifestOverhead + Planner.MediaDescriptorOverhead
  private val Media2Space = 8L * MiB
  private val PartSize = 4096L
  private val SetId = "8f31c2"

  private case class Env(src: Path, media1: Path, media2: Path, work: Path, config: transfer.config.Config, set: TransferSet, plan: transfer.planning.Plan)

  /** Готовое окружение: исходные данные, два носителя, рабочий каталог и план. */
  private def withEnv[A](body: Env => A): A =
    val root = Files.createTempDirectory("transfer-writer")
    try
      val src = Files.createDirectories(root.resolve("src"))
      val media1 = Files.createDirectories(root.resolve("media1"))
      val media2 = Files.createDirectories(root.resolve("media2"))
      val work = Files.createDirectories(root.resolve("work"))
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
      TestFs.createFile(src.resolve("sub/c.txt"), TestFs.pattern(2000))
      val set = Collector.collect(SetId, "vacation-archive", List(SelectedItem.Directory(src.toString, "src")))
        .fold(err => fail(err.message), identity)
      val plan = Planner.buildPlan(set, mediaFor(media1, Media1Space, "BLUE-USB") :: mediaFor(media2, Media2Space, "OLD-FLASH") :: Nil, config)
        .fold(err => fail(err.message), identity)
      body(Env(src, media1, media2, work, config, set, plan))
    finally TestFs.deleteTree(root)

  private def mediaFor(path: Path, available: Long, label: String): Media =
    Media(label, path.toString, available, available, writable = true, None)

  private def mediaList(env: Env): List[Media] =
    mediaFor(env.media1, Media1Space, "BLUE-USB") :: mediaFor(env.media2, Media2Space, "OLD-FLASH") :: Nil

  private def write(env: Env, media: List[Media]): WriteProgress =
    Writer.writeSet(env.set, env.plan, media, env.config).fold(err => fail(err.message), identity)

  private def partName(number: Int): String = f"vacation-archive.part$number%03d"

  private def dataStream(env: Env): Array[Byte] =
    TestFs.concat(List(env.src.resolve("a.txt"), env.src.resolve("b.bin"), env.src.resolve("empty.txt"), env.src.resolve("sub/c.txt")))

  test("write: набор записывается целиком и все части проверены") {
    withEnv { env =>
      val progress = write(env, mediaList(env))

      assertEquals(progress.completedMedia, 2)
      assertEquals(progress.totalMedia, 2)
      assertEquals(progress.verifiedParts, List(1, 2, 3))
      assertEquals(progress.lastVerifiedPart, Some(3))
      assertEquals(progress.nextPart, None)
      assert(progress.manifest.completed)
    }
  }

  test("write: части лежат на запланированных носителях и имеют ожидаемый размер") {
    withEnv { env =>
      write(env, mediaList(env))

      assertEquals(env.plan.parts.map(_.mediaNumber), List(1, 2, 2))
      assertEquals(Files.size(env.media1.resolve(partName(1))), 4096L)
      assertEquals(Files.size(env.media2.resolve(partName(2))), 4096L)
      assertEquals(Files.size(env.media2.resolve(partName(3))), 1808L)
    }
  }

  test("write: склеенные части в порядке номеров дают исходные данные") {
    withEnv { env =>
      write(env, mediaList(env))

      val actual = TestFs.concat(List(env.media1.resolve(partName(1)), env.media2.resolve(partName(2)), env.media2.resolve(partName(3))))
      assertEquals(actual.toList, dataStream(env).toList)
    }
  }

  test("write: на каждом носителе появляется служебное описание") {
    withEnv { env =>
      write(env, mediaList(env))

      val first = ManifestIO.readDescriptor(env.media1, env.config.mediaDescriptorFile).fold(err => fail(err.message), identity)
      val second = ManifestIO.readDescriptor(env.media2, env.config.mediaDescriptorFile).fold(err => fail(err.message), identity)

      assertEquals(first.map(_.setId), Some(SetId))
      assertEquals(first.map(_.mediaNumber), Some(1))
      assertEquals(first.map(_.partNumbers), Some(List(1)))
      assertEquals(first.map(_.completed), Some(true))
      assertEquals(second.map(_.mediaNumber), Some(2))
      assertEquals(second.map(_.partNumbers), Some(List(2, 3)))
    }
  }

  test("write: манифест сохраняется на первом носителе и в рабочем каталоге") {
    withEnv { env =>
      val progress = write(env, mediaList(env))

      val onMedia = ManifestIO.readManifest(env.media1.resolve(env.config.manifestFile)).fold(err => fail(err.message), identity)
      val inWork = ManifestIO.readManifest(env.work.resolve(env.config.manifestFile)).fold(err => fail(err.message), identity)

      assertEquals(onMedia.completed, true)
      assertEquals(inWork.completed, true)
      assertEquals(onMedia.parts.map(_.number), progress.manifest.parts.map(_.number))
      assertEquals(onMedia.dirs, progress.manifest.dirs)
    }
  }

  test("write: в манифесте есть контрольные суммы файлов и частей") {
    withEnv { env =>
      val progress = write(env, mediaList(env))

      assert(progress.manifest.files.forall(_.checksum.nonEmpty))
      progress.manifest.files.foreach { file =>
        val abs = env.src.getParent.resolve(file.relPath)
        assertEquals(Checksum.ofFile(abs, env.config.checksumAlgorithm).fold(err => fail(err.message), identity), file.checksum)
      }
      assert(progress.manifest.parts.forall(_.checksum.nonEmpty))
      assertEquals(progress.manifest.files.map(_.relPath).sorted, List("src/a.txt", "src/b.bin", "src/empty.txt", "src/sub/c.txt"))
    }
  }

  test("write: после завершения состояние прерванной записи удаляется") {
    withEnv { env =>
      write(env, mediaList(env))

      assertEquals(SessionIO.load(env.work).fold(err => fail(err.message), identity), None)
    }
  }

  test("write: носитель, занятый другим набором, отвергается") {
    withEnv { env =>
      ManifestIO.writeDescriptor(env.media2, MediaDescriptor("ab12cd", "other-set", 2, 2, List(2, 3), completed = true), env.config.mediaDescriptorFile)
        .fold(err => fail(err.message), identity)

      val result = Writer.writeSet(env.set, env.plan, mediaList(env), env.config)

      assert(result.isLeft)
      assert(result.swap.toOption.exists(_.isInstanceOf[AppError.WrongMedia]))
    }
  }

  test("write: изменение исходных данных после планирования обнаруживается") {
    withEnv { env =>
      TestFs.createFile(env.src.resolve("a.txt"), TestFs.pattern(4000))

      val result = Writer.writeSet(env.set, env.plan, mediaList(env), env.config)

      assert(result.isLeft)
      assert(result.swap.toOption.exists(_.isInstanceOf[AppError.SourceChanged]))
    }
  }

  test("verify: проверка записанного набора проходит успешно") {
    withEnv { env =>
      val progress = write(env, mediaList(env))

      val report = Verifier.verify(progress.manifest, List(env.media1, env.media2), env.config).fold(err => fail(err.message), identity)

      assert(!report.failed)
      assertEquals(report.results.map(_.mediaNumber), List(1, 2))
      assert(report.results.forall(_.ok))
    }
  }

  test("verify: потерянная часть обнаруживается") {
    withEnv { env =>
      val progress = write(env, mediaList(env))
      Files.delete(env.media2.resolve(partName(2)))

      val report = Verifier.verify(progress.manifest, List(env.media1, env.media2), env.config).fold(err => fail(err.message), identity)

      assert(report.failed)
      assert(report.results.flatMap(_.failures).exists(_.isInstanceOf[AppError.PartMissing]))
    }
  }

  test("verify: испорченная часть обнаруживается по контрольной сумме") {
    withEnv { env =>
      val progress = write(env, mediaList(env))
      TestFs.createFile(env.media2.resolve(partName(3)), TestFs.pattern(1808))

      val report = Verifier.verify(progress.manifest, List(env.media1, env.media2), env.config).fold(err => fail(err.message), identity)

      assert(report.failed)
      assert(report.results.flatMap(_.failures).exists(_.isInstanceOf[AppError.ChecksumMismatch]))
    }
  }

  test("verify: лишняя часть на носителе обнаруживается") {
    withEnv { env =>
      val progress = write(env, mediaList(env))
      TestFs.createFile(env.media1.resolve(partName(7)), TestFs.pattern(16))

      val report = Verifier.verify(progress.manifest, List(env.media1, env.media2), env.config).fold(err => fail(err.message), identity)

      assert(report.failed)
      assert(report.results.flatMap(_.failures).exists(_.isInstanceOf[AppError.UnexpectedPart]))
    }
  }

  test("verify: незавершённый набор не проверяется") {
    withEnv { env =>
      val progress = write(env, mediaList(env))

      val result = Verifier.verify(progress.manifest.copy(completed = false), List(env.media1, env.media2), env.config)

      assertEquals(result, Left(AppError.SetNotCompleted("vacation-archive")))
    }
  }

  test("continue: прерванная запись продолжается, проверенные части не перезаписываются") {
    withEnv { env =>
      // Второй носитель недоступен: запись прерывается после первого носителя.
      val missing = env.work.getParent.resolve("missing-media")
      val interrupted = Writer.writeSet(env.set, env.plan, mediaFor(env.media1, Media1Space, "BLUE-USB") :: mediaFor(missing, Media2Space, "OLD-FLASH") :: Nil, env.config)
      assert(interrupted.isLeft)

      val session = SessionIO.load(env.work).fold(err => fail(err.message), identity)
      assertEquals(session.map(_.verifiedParts), Some(List(1)))
      assertEquals(session.map(_.setId), Some(SetId))

      val part1 = env.media1.resolve(partName(1))
      val stamp = Files.getLastModifiedTime(part1)
      val sum = Checksum.ofFile(part1, env.config.checksumAlgorithm).fold(err => fail(err.message), identity)

      val progress = Writer.resume(env.set, env.plan, mediaList(env), env.config).fold(err => fail(err.message), identity)

      assertEquals(progress.verifiedParts, List(1, 2, 3))
      assertEquals(progress.completedMedia, 2)
      assert(progress.manifest.completed)
      // уже проверенная часть не тронута
      assertEquals(Files.getLastModifiedTime(part1), stamp)
      assertEquals(Checksum.ofFile(part1, env.config.checksumAlgorithm).fold(err => fail(err.message), identity), sum)
      // данные на месте
      val actual = TestFs.concat(List(part1, env.media2.resolve(partName(2)), env.media2.resolve(partName(3))))
      assertEquals(actual.toList, dataStream(env).toList)
    }
  }

  test("continue: удалённая проверенная часть создаётся заново") {
    withEnv { env =>
      val missing = env.work.getParent.resolve("missing-media")
      Writer.writeSet(env.set, env.plan, mediaFor(env.media1, Media1Space, "BLUE-USB") :: mediaFor(missing, Media2Space, "OLD-FLASH") :: Nil, env.config)
      Files.delete(env.media1.resolve(partName(1)))

      val progress = Writer.resume(env.set, env.plan, mediaList(env), env.config).fold(err => fail(err.message), identity)

      assertEquals(progress.verifiedParts, List(1, 2, 3))
      assertEquals(Files.size(env.media1.resolve(partName(1))), 4096L)
      val actual = TestFs.concat(List(env.media1.resolve(partName(1)), env.media2.resolve(partName(2)), env.media2.resolve(partName(3))))
      assertEquals(actual.toList, dataStream(env).toList)
    }
  }

  test("continue: без сохранённого состояния работа невозможна") {
    withEnv { env =>
      val result = Writer.resume(env.set, env.plan, mediaList(env), env.config)

      assertEquals(result.swap.toOption.map(_.isInstanceOf[AppError.MissingState]), Some(true))
    }
  }
