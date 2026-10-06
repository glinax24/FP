package transfer.operations

import transfer.domain.*
import transfer.planning.Planner

/** Проверка чистого распределения данных набора по частям (§5). */
class PartPackerSuite extends munit.FunSuite:

  private def file(rel: String, size: Long): SourceEntry.File =
    SourceEntry.File(absPath = s"/data/$rel", relPath = rel, size = size, modifiedAt = "2026-10-14T18:30:00Z", readable = true)

  private def part(number: Int, size: Long): Part =
    Part(number, Planner.partFileName("{setName}.part{number}", "vacation-archive", number), mediaNumber = 1, size = size, checksum = None)

  private def pack(files: List[SourceEntry.File], parts: List[Part]): List[PartContent] =
    PartPacker.pack(files, parts).fold(err => fail(err.message), identity)

  test("pack: крупный файл разбивается на фрагменты по границам частей") {
    val contents = pack(List(file("movie.mkv", 250L)), List(part(1, 100L), part(2, 100L), part(3, 50L)))

    assertEquals(contents.map(_.number), List(1, 2, 3))
    assertEquals(contents.map(_.size), List(100L, 100L, 50L))
    assertEquals(
      contents.map(_.slices.map(s => (s.offset, s.length))),
      List(List((0L, 100L)), List((100L, 100L)), List((200L, 50L)))
    )
  }

  test("pack: несколько файлов складываются в части по порядку") {
    val contents = pack(List(file("a.txt", 30L), file("b.txt", 40L), file("c.txt", 30L)), List(part(1, 60L), part(2, 40L)))

    assertEquals(
      contents.map(_.slices.map(s => (s.relPath, s.offset, s.length))),
      List(
        List(("a.txt", 0L, 30L), ("b.txt", 0L, 30L)),
        List(("b.txt", 30L, 10L), ("c.txt", 0L, 30L))
      )
    )
  }

  test("pack: суммарный объём частей равен объёму набора") {
    val contents = pack(List(file("a.txt", 3000L), file("b.bin", 5000L), file("sub/c.txt", 2000L)), List(part(1, 4096L), part(2, 4096L), part(3, 1808L)))

    assertEquals(contents.map(_.size).sum, 10000L)
    assertEquals(contents.map(_.slices.map(_.length).sum), List(4096L, 4096L, 1808L))
  }

  test("pack: файл ровно по границе частей не разрывается") {
    val contents = pack(List(file("a.txt", 100L), file("b.txt", 100L)), List(part(1, 100L), part(2, 100L)))

    assertEquals(contents.map(_.slices.map(_.relPath)), List(List("a.txt"), List("b.txt")))
  }

  test("pack: файлы нулевой длины не занимают места в частях") {
    val contents = pack(List(file("empty.txt", 0L), file("a.txt", 10L)), List(part(1, 10L)))

    assertEquals(contents.map(_.slices.map(_.relPath)), List(List("a.txt")))
  }

  test("pack: порядок файлов детерминирован — по относительному пути") {
    val byName = pack(List(file("b.txt", 10L), file("a.txt", 10L)), List(part(1, 20L)))
    val byNameReversed = pack(List(file("a.txt", 10L), file("b.txt", 10L)), List(part(1, 20L)))

    assertEquals(byName.flatMap(_.slices).map(_.relPath), List("a.txt", "b.txt"))
    assertEquals(byName.flatMap(_.slices).map(_.relPath), byNameReversed.flatMap(_.slices).map(_.relPath))
  }

  test("pack: объём набора изменился после планирования") {
    val result = PartPacker.pack(List(file("a.txt", 999L)), List(part(1, 100L)))

    assert(result.isLeft)
    assert(result.swap.toOption.exists(_.isInstanceOf[AppError.SourceChanged]))
  }

  test("pack: пустой набор не создаёт частей") {
    assertEquals(PartPacker.pack(Nil, Nil), Right(Nil))
  }

  test("packFrom: продолжение с середины файла") {
    val contents = PartPacker
      .packFrom(List(file("a.txt", 100L), file("b.txt", 100L)), List(part(4, 50L)), skip = 150L)
      .fold(err => fail(err.message), identity)

    assertEquals(contents.map(_.number), List(4))
    assertEquals(contents.flatMap(_.slices).map(s => (s.relPath, s.offset, s.length)), List(("b.txt", 50L, 50L)))
  }

  test("packFrom: продолжение с границы файла") {
    val contents = PartPacker
      .packFrom(List(file("a.txt", 100L), file("b.txt", 100L)), List(part(3, 50L), part(4, 50L)), skip = 100L)
      .fold(err => fail(err.message), identity)

    assertEquals(contents.map(_.number), List(3, 4))
    assertEquals(
      contents.map(_.slices.map(s => (s.offset, s.length))),
      List(List((0L, 50L)), List((50L, 50L)))
    )
  }

  test("packFrom: с skip = 0 совпадает с pack") {
    val files = List(file("a.txt", 30L), file("b.txt", 40L))
    val parts = List(part(1, 60L), part(2, 10L))
    val packed = pack(files, parts)
    val fromZero = PartPacker.packFrom(files, parts, skip = 0L).fold(err => fail(err.message), identity)
    assertEquals(packed.map(_.slices), fromZero.map(_.slices))
  }

  test("packFrom: несовпадение объёма остатка и частей — ошибка") {
    val result = PartPacker.packFrom(List(file("a.txt", 100L), file("b.txt", 100L)), List(part(4, 100L)), skip = 0L)
    assert(result.isLeft)
    assert(result.swap.toOption.exists(_.isInstanceOf[AppError.SourceChanged]))
  }

  test("packFrom: отрицательная точка продолжения — ошибка") {
    val result = PartPacker.packFrom(List(file("a.txt", 100L)), List(part(4, 100L)), skip = -1L)
    assert(result.isLeft)
    assert(result.swap.toOption.exists(_.isInstanceOf[AppError.SourceChanged]))
  }
