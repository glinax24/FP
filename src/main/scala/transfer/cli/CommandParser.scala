package transfer.cli

import transfer.domain.AppError
import transfer.util.SizeFormat

// Чистый разбор строки(те введенной команды пользователя) в команду
object CommandParser:

  def parse(line: String): Either[AppError, Command] =
    val trimmed = line.trim
    if trimmed.isEmpty then Left(AppError.UnknownCommand(line))
    else
      val (verb, argRaw) = trimmed.span(_ != ' ')//span проходится по строке пока символ не равен пробелу записываем все в verb, то что после пообела в argRaw
      val arg = argRaw.trim // убирает пробелы в начале и конце строки
      verb.toLowerCase match //в нижний регистр все
        case "create-set"    => argOf(verb, arg)(Command.CreateSet.apply)
        case "add-file"      => argOf(verb, arg)(Command.AddFile.apply)
        case "add-directory" => argOf(verb, arg)(Command.AddDirectory.apply)
        case "remove-source" => argOf(verb, arg)(Command.RemoveSource.apply)
        case "sources"       => Right(Command.ShowSources)
        case "add-media"     => argOf(verb, arg)(Command.AddMedia.apply)
        case "remove-media"  => argOf(verb, arg)(Command.RemoveMedia.apply)
        case "move-media"    => intArgs(verb, arg).map(Command.MoveMedia.apply)
        case "rescan-media"  => Right(Command.RescanMedia)
        case "limit-media"   => splitRight(arg)
        case "label-media"   => labelArg(verb, arg)
        case "connect-media" => connectArg(verb, arg)
        case "media"         => Right(Command.ShowMedia)
        case "plan"          => Right(Command.BuildPlan)
        case "write-confirm" => Right(Command.ConfirmWrite)
        case "write"         => Right(Command.Write)
        case "continue"      => Right(Command.Continue)
        case "cancel-write"  => Right(Command.CancelWrite)
        case "rebuild"       => Right(Command.Rebuild)
        case "exclude-unreadable" => Right(Command.ExcludeUnreadable)
        case "recheck-sources"    => Right(Command.RecheckSources)
        case "exclude-changed"    => Right(Command.ExcludeChanged)
        case "verify"        => Right(Command.Verify)
        case "restore"       => argOf(verb, arg)(Command.Restore.apply)
        case "open-manifest" => argOf(verb, arg)(Command.OpenManifest.apply)
        case "manifest"      => Right(Command.ShowManifest)
        case "report"        => Right(Command.ShowReport)
        case "reload"        => Right(Command.Reload)
        case "exit" | "quit" => Right(Command.Exit)
        case "help" | "?"    => Right(Command.Help)
        case _               => Left(AppError.UnknownCommand(line))

  //проверяет, что команда получила аргумент, и если получила — создаёт из этого аргумента нужную команду.
  private def argOf[A](verb: String, arg: String)(mk: String => A): Either[AppError, A] = 
    if arg.isEmpty then Left(AppError.UnknownCommand(s"$verb <аргумент>"))
    else Right(mk(arg))

  //если больше одного аргумента
  private def intArgs(verb: String, arg: String): Either[AppError, (Int, Int)] =
    arg.split("\\s+").toList match //разбивает строку по одному или нескольким  пробелам и делает List("3","1")
      case a :: b :: Nil => // ожидаем список ровно из двух эл-ов. Nil - конец списка
        (a.toIntOption, b.toIntOption) match //из строк в числа
          case (Some(x), Some(y)) => Right((x, y))
          case _                  => Left(AppError.UnknownCommand(s"$verb: нужны целые числа, получено «$arg»"))//если хотя бы один из элементов не число, ну или другие ошибкм
      case _ => Left(AppError.UnknownCommand(s"$verb <аргумент1> <аргумент2>"))//если элементов не два

  //связана с label-media. Название носителя может содержать пробелы, а путь нет.путь идёт первым
  private def labelArg(verb: String, arg: String): Either[AppError, Command] =
    arg.split("\\s+", 2).toList match
      case path :: label :: Nil if path.nonEmpty && label.trim.nonEmpty => Right(Command.LabelMedia(path, label.trim))
      case _                                                        => Left(AppError.UnknownCommand("label-media <путь> <название>"))

  //связана с connect-media. Путь к носителю можно не указывать: приложение уточнит его у пользователя. */
  private def connectArg(verb: String, arg: String): Either[AppError, Command] =
    arg.split("\\s+", 2).toList match
      case Nil | List("") => Left(AppError.UnknownCommand("connect-media <номер> [<путь>]"))
      case number :: rest =>
        number.toIntOption match
          case None                       => Left(AppError.UnknownCommand(s"connect-media: номер носителя должен быть целым числом, получено «$number»"))
          case Some(n) if rest.isEmpty    => Right(Command.ConnectMedia(n, ""))
          case Some(n)                    => Right(Command.ConnectMedia(n, rest.head.trim))

  //шаблон для limit-media. те аргумент(arg): path\\пробел-ы\\число с точкой или запятой\\пробел-ы\\гб,кб и тд(число+гб - это sizeRaw)
  private val sizeSuffix = "(?i)^(.*?)\\s+(\\d+(?:[.,]\\d+)?\\s*(?:B|KB|MB|GB|TB|Б|КБ|МБ|ГБ|ТБ))$".r

  //для limit-media разделяем аргумент на путь и размер, чтобы затем передать это в limit-media
  private def splitRight(arg: String): Either[AppError, Command] =
    arg match //если arg подходит под трафарет sizeSuffix
      case sizeSuffix(path, sizeRaw) if path.nonEmpty =>
        SizeFormat.parseSize(sizeRaw) match //перевод в байты
          case Some(bytes) => Right(Command.LimitMedia(path.trim, bytes))
          case None        => Left(AppError.UnknownCommand(s"limit-media: не удалось распознать размер: $sizeRaw"))
      case _ => Left(AppError.UnknownCommand("limit-media <путь> <размер>")) //если строка совсем не подошла под трафарет
