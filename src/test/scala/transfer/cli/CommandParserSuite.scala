package transfer.cli

import transfer.domain.AppError

class CommandParserSuite extends munit.FunSuite:

  test("create-set с именем набора") {
    assertEquals(CommandParser.parse("create-set vacation-archive"), Right(Command.CreateSet("vacation-archive")))
  }

  test("add-file с путём, содержащим пробелы") {
    assertEquals(CommandParser.parse("add-file C:\\My Files\\a.zip"), Right(Command.AddFile("C:\\My Files\\a.zip")))
  }

  test("команды без аргументов") {
    assertEquals(CommandParser.parse("sources"), Right(Command.ShowSources))
    assertEquals(CommandParser.parse("plan"), Right(Command.BuildPlan))
    assertEquals(CommandParser.parse("write"), Right(Command.Write))
    assertEquals(CommandParser.parse("verify"), Right(Command.Verify))
    assertEquals(CommandParser.parse("manifest"), Right(Command.ShowManifest))
    assertEquals(CommandParser.parse("reload"), Right(Command.Reload))
    assertEquals(CommandParser.parse("exit"), Right(Command.Exit))
    assertEquals(CommandParser.parse("rescan-media"), Right(Command.RescanMedia))
    assertEquals(CommandParser.parse("write-confirm"), Right(Command.ConfirmWrite))
    assertEquals(CommandParser.parse("cancel-write"), Right(Command.CancelWrite))
    assertEquals(CommandParser.parse("rebuild"), Right(Command.Rebuild))
    assertEquals(CommandParser.parse("exclude-unreadable"), Right(Command.ExcludeUnreadable))
    assertEquals(CommandParser.parse("recheck-sources"), Right(Command.RecheckSources))
  }

  test("remove-source и remove-media с путём, содержащим пробелы") {
    assertEquals(
      CommandParser.parse("remove-source C:\\My Files\\a.zip"),
      Right(Command.RemoveSource("C:\\My Files\\a.zip"))
    )
    assertEquals(
      CommandParser.parse("remove-media E:\\My Drive"),
      Right(Command.RemoveMedia("E:\\My Drive"))
    )
  }

  test("move-media с позициями") {
    assertEquals(CommandParser.parse("move-media 2 1"), Right(Command.MoveMedia(2, 1)))
    assert(CommandParser.parse("move-media 3").isLeft)
  }

  test("limit-media: путь с пробелами и размер с суффиксом") {
    assertEquals(
      CommandParser.parse("limit-media E:\\My Drive 2 GB"),
      Right(Command.LimitMedia("E:\\My Drive", 2L * 1024 * 1024 * 1024))
    )
    assertEquals(
      CommandParser.parse("limit-media E:\\ 500 MB"),
      Right(Command.LimitMedia("E:\\", 500L * 1024 * 1024))
    )
    assert(CommandParser.parse("limit-media E:\\ 500").isLeft)
  }

  test("open-manifest с путём") {
    assertEquals(CommandParser.parse("open-manifest D:\\Media\\manifest.json"), Right(Command.OpenManifest("D:\\Media\\manifest.json")))
  }

  test("restore с каталогом восстановления") {
    assertEquals(CommandParser.parse("restore D:\\Restored"), Right(Command.Restore("D:\\Restored")))
  }

  test("неизвестная команда") {
    assert(CommandParser.parse("fly").isLeft)
  }

  test("команда без обязательного аргумента") {
    assert(CommandParser.parse("add-file").isLeft)
  }