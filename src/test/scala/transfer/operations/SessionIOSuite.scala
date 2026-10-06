package transfer.operations

import transfer.TestFs

import java.nio.file.Files

/** Сохранение и чтение состояния прерванной записи (§8). */
class SessionIOSuite extends munit.FunSuite:

  private def withWorkDir[A](body: java.nio.file.Path => A): A =
    val work = Files.createTempDirectory("transfer-session")
    try body(work)
    finally TestFs.deleteTree(work)

  private val state = SessionState(
    setId = "8f31c2",
    setName = "vacation-archive",
    completedMedia = 1,
    totalMedia = 3,
    verifiedParts = List(1, 2),
    nextPart = Some(3)
  )

  test("save и load: состояние сохраняется и читается без потерь") {
    withWorkDir { work =>
      SessionIO.save(state, work).fold(err => fail(err.message), identity)

      assertEquals(SessionIO.load(work).fold(err => fail(err.message), identity), Some(state))
    }
  }

  test("load: если записи ещё не было, состояние отсутствует") {
    withWorkDir { work =>
      assertEquals(SessionIO.load(work).fold(err => fail(err.message), identity), None)
    }
  }

  test("save: повторное сохранение перезаписывает состояние") {
    withWorkDir { work =>
      SessionIO.save(state, work).fold(err => fail(err.message), identity)
      val next = state.copy(completedMedia = 2, verifiedParts = List(1, 2, 3, 4), nextPart = Some(5))
      SessionIO.save(next, work).fold(err => fail(err.message), identity)

      assertEquals(SessionIO.load(work).fold(err => fail(err.message), identity), Some(next))
    }
  }

  test("clear: состояние удаляется после успешного завершения") {
    withWorkDir { work =>
      SessionIO.save(state, work).fold(err => fail(err.message), identity)
      SessionIO.clear(work).fold(err => fail(err.message), identity)

      assertEquals(SessionIO.load(work).fold(err => fail(err.message), identity), None)
    }
  }

  test("load: повреждённый файл состояния не приводит к аварии") {
    withWorkDir { work =>
      Files.writeString(work.resolve(SessionIO.FileName), "{ это не json")

      val result = SessionIO.load(work)

      assert(result.isLeft)
      assert(result.swap.toOption.exists(_.isInstanceOf[transfer.domain.AppError.MissingState]))
    }
  }
