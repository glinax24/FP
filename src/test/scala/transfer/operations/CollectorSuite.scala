package transfer.operations

import transfer.TestFs.deleteTree
import transfer.domain.*

import java.nio.file.Files

class CollectorSuite extends munit.FunSuite:

  test("collect: сохраняет структуру каталога и относительные пути") {
    val tmp = Files.createTempDirectory("collector-test")
    val docs = Files.createDirectories(tmp.resolve("docs"))
    val tables = Files.createDirectories(docs.resolve("tables"))
    Files.writeString(docs.resolve("report.docx"), "hello")
    Files.writeString(tables.resolve("budget.xlsx"), "world")
    try
      val items = List(SelectedItem.Directory(docs.toString, "docs"))
      val set = Collector.collect("abc123", "t", items).fold(err => fail(err.message), identity)
      assertEquals(set.fileCount, 2)
      assertEquals(set.directoryCount, 1)
      val relPaths = set.entries.collect { case f: SourceEntry.File => f.relPath }
      assert(relPaths.contains("docs/report.docx"))
      assert(relPaths.contains("docs/tables/budget.xlsx"))
    finally deleteTree(tmp)
  }

  test("collect: обнаруживает конфликт относительных путей") {
    val tmp = Files.createTempDirectory("collector-test-2")
    val a = Files.writeString(tmp.resolve("a.txt"), "aaa")
    val b = Files.writeString(tmp.resolve("b.txt"), "bbb")
    try
      val items =
        List(SelectedItem.File(a.toString, "same.txt"), SelectedItem.File(b.toString, "same.txt"))
      val set = Collector.collect("abc123", "t", items).fold(err => fail(err.message), identity)
      assertEquals(set.pathConflicts, List("same.txt"))
    finally deleteTree(tmp)
  }

  test("collect: отсутствующий источник — ошибка") {
    val result = Collector.collect("abc123", "t", List(SelectedItem.File("Z:\\нет-такого.txt", "нет.txt")))
    assert(result.isLeft)
    assert(result.swap.toOption.exists(e => e.isInstanceOf[AppError.SourceNotFound]))
  }

  test("collect: один и тот же файл не учитывается дважды") {
    val tmp = Files.createTempDirectory("collector-dup")
    val src = Files.createDirectories(tmp.resolve("src"))
    val file = Files.writeString(src.resolve("a.txt"), "данные")
    val size = Files.size(file)
    try
      // Файл добавлен явно, а затем повторно найден обходом каталога.
      val fileFirst =
        Collector
          .collect("abc123", "t", List(SelectedItem.File(file.toString, "a.txt"), SelectedItem.Directory(src.toString, "src")))
          .fold(err => fail(err.message), identity)
      assertEquals(fileFirst.fileCount, 1, "файл не удваивается")
      assertEquals(fileFirst.totalSize, size, "объём не удваивается")
      assertEquals(fileFirst.pathConflicts, Nil, "дубликат не считается конфликтом")
      assertEquals(fileFirst.dropped, Nil)

      // То же самое, но каталог добавлен раньше файла — порядок не должен влиять.
      val dirFirst =
        Collector
          .collect("abc123", "t", List(SelectedItem.Directory(src.toString, "src"), SelectedItem.File(file.toString, "a.txt")))
          .fold(err => fail(err.message), identity)
      assertEquals(dirFirst.fileCount, 1, "порядок добавления не влияет")
      assertEquals(dirFirst.totalSize, size)

      // Явное повторение одной и той же команды add-file.
      val twice =
        Collector
          .collect("abc123", "t", List(SelectedItem.File(file.toString, "a.txt"), SelectedItem.File(file.toString, "a.txt")))
          .fold(err => fail(err.message), identity)
      assertEquals(twice.fileCount, 1, "повторный add-file того же файла игнорируется")
      assertEquals(twice.totalSize, size)

      // Один и тот же каталог дважды.
      val dirTwice =
        Collector
          .collect("abc123", "t", List(SelectedItem.Directory(src.toString, "src"), SelectedItem.Directory(src.toString, "src")))
          .fold(err => fail(err.message), identity)
      assertEquals(dirTwice.fileCount, 1)
      assertEquals(dirTwice.directoryCount, 0)
    finally deleteTree(tmp)
  }
