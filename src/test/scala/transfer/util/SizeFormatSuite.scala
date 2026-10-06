package transfer.util

class SizeFormatSuite extends munit.FunSuite:

  test("parseSize: учитывает суффиксы") {
    assertEquals(SizeFormat.parseSize("500 MB"), Some(500L * 1024 * 1024))
    assertEquals(SizeFormat.parseSize("2 GB"), Some(2L * 1024 * 1024 * 1024))
    assertEquals(SizeFormat.parseSize("64 MB"), Some(64L * 1024 * 1024))
    assertEquals(SizeFormat.parseSize("512 B"), Some(512L))
    assertEquals(SizeFormat.parseSize("100 КБ"), Some(100L * 1024))
  }

  test("parseSize: неверный ввод — None") {
    assertEquals(SizeFormat.parseSize("abc"), None)
    assertEquals(SizeFormat.parseSize(""), None)
    assertEquals(SizeFormat.parseSize("-5 GB"), None)
  }

  test("humanReadable: корректные единицы") {
    assertEquals(SizeFormat.humanReadable(0), "0 Б")
    assertEquals(SizeFormat.humanReadable(1024), "1,0 КБ")
    assertEquals(SizeFormat.humanReadable(SizeFormat.GiB), "1,0 ГБ")
  }