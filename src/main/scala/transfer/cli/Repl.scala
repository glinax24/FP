package transfer.cli

import transfer.app.{App, AppState}
import transfer.config.Config
import transfer.domain.{AppError, Media}
import transfer.operations.{MediaRequest, SessionIO}
import transfer.util.Logger

import java.nio.file.{Files, Path, Paths}
import scala.util.control.NonFatal

//Repl отвечает за консольное взаимодействие с пользователем:
  // получает команды, передаёт их в App и выводит результат

/** Цикл текстовых команд  */
object Repl: // В нём находится логика консольного взаимодействия с пользователем

  //Запускает главный цикл программы: показывает команды, читает ввод пользователя, выполняет команды и обновляет состояние
  def run(config: Config, configPath: String): Unit =
    println(Command.usage) // «Вот какие команды ты можешь вводить».
    println()
    detectInterrupted(config).foreach(println) // если осталась незавершённая запись, сообщаем об этом 
    var state: AppState = App.empty(config, configPath) // текущее состояние программы
    Logger.info("Приложение запущено, конфигурация загружена", config.logFile)
    var running = true
    while running do
      print("> ")
      val line = scala.io.StdIn.readLine() // readLine() ждёт, пока пользователь что-нибудь напишет и нажмёт Enter
      if line == null then
        running = false // если ввод закончился, завершаем работу
        Logger.info("Завершение работы.", config.logFile)
      else
        CommandParser.parse(line) match // превращаем строку в команду программы
          case Right(Command.Exit) =>
            running = false
            Logger.info("Завершение работы.", config.logFile)
          case Right(cmd) =>
            // Команда выполняется с интерактивным доступом к носителям и подтверждением.
            execute(cmd, state, config) match
              case Right((next, message)) =>
                state = next
                println(message)
                Logger.info(s"${describe(cmd)} — выполнено", state.config.logFile)
              case Left(err) =>
                println(s"Ошибка: ${err.message}")
                Logger.error(s"Команда ${describe(cmd)}: ${err.message}", state.config.logFile)
          case Left(err) =>
            println(s"Ошибка: ${err.message}")
            Logger.warn(err.message, state.config.logFile)

  //Передаёт команду в App на выполнение и перехватывает внутренние ошибки, чтобы программа не завершилась
  private def execute(cmd: Command, state: AppState, config: Config): Either[AppError, (AppState, String)] =
    try App(cmd, state, askConfirm, Some(ConsoleMediaRequest))
    catch
      case NonFatal(e) =>
        val detail = s"внутренняя ошибка: ${e.getClass.getSimpleName}${Option(e.getMessage).map(m => s": $m").getOrElse("")}"
        Logger.error(s"Команда ${describe(cmd)} — $detail", state.config.logFile)
        Left(AppError.MissingState(detail))

  //Проверяет, осталась ли незавершённая запись, и сообщает пользователю, как её продолжить
  private def detectInterrupted(config: Config): Option[String] =
    SessionIO.load(Paths.get(config.workDirectory)).toOption.flatten.map { session =>
      s"Обнаружена незавершённая операция:\n\n" +
      s"Набор: ${session.setName}\n" +
      s"Завершено носителей: ${session.completedMedia} из ${session.totalMedia}\n" +
      s"Последняя проверенная часть: ${session.verifiedParts.lastOption.getOrElse(0)}\n" +
      s"Следующая часть: ${session.nextPart.map(_.toString).getOrElse("-")}\n\n" +
      "Для продолжения записи выполните create-set <имя>, добавьте те же исходные файлы и каталоги, затем continue."
    }

  //Запрашивает у пользователя подтверждение «да/нет» и возвращает true или false
  private def askConfirm(question: String): Boolean =
    println(s"$question [да/нет] ")
    val answer = scala.io.StdIn.readLine()
    answer != null && Set("y", "yes", "д", "да", "true", "1").contains(answer.trim.toLowerCase)

  //Запрашивает строку и повторяет запрос, пока пользователь не введёт непустое значение
  private def ask(question: String): String =
    var answer: String = null
    while answer == null || answer.trim.isEmpty do
      print(question)
      Console.flush()
      answer = scala.io.StdIn.readLine()
      if answer == null then
        println()
        answer = "y"
    answer.trim

  //
   //Запрашивает путь к нужному носителю и проверяет, существует ли по этому пути каталог
   //Пустой ответ означает «носитель уже подключён», путь берётся из плана.
  
  private object ConsoleMediaRequest extends MediaRequest:
    override def path(number: Int, total: Int, media: Media): Either[AppError, String] =
      val hint = if media.path.isEmpty then "" else s" [${media.path}]"
      val answer = askLine(s"Носитель $number из $total (${media.label})$hint. Введите путь: ")
      if answer.isEmpty then
        if media.path.nonEmpty && Files.isDirectory(Paths.get(media.path)) then Right(media.path)
        else Left(AppError.MediaUnavailable(s"носитель $number (${media.label}): путь не указан, носитель не подключён"))
      else if Files.isDirectory(Paths.get(answer)) then Right(answer)
      else
        val retry = askLine(s"Каталог $answer недоступен. Введите путь заново или «skip» [skip]: ")
        if retry.isEmpty || retry.equalsIgnoreCase("skip") then
          Left(AppError.MediaUnavailable(s"носитель $number (${media.label}): подключение отменено"))
        else if Files.isDirectory(Paths.get(retry)) then Right(retry)
        else Left(AppError.MediaUnavailable(s"носитель $number (${media.label}): каталог $retry недоступен"))

  //Считывает одну строку из консоли, при этом пустой ответ разрешён
  private def askLine(question: String): String =
    print(question)
    Console.flush()
    val answer = scala.io.StdIn.readLine()
    if answer == null then
      println()
      ""
    else answer.trim

  //Превращает объект команды в короткую текстовую запись для журнала, 
  //например Command.AddFile("test.txt") в add-file test.txt
  private def describe(cmd: Command): String = cmd match
    case Command.CreateSet(name)          => s"create-set $name"
    case Command.AddFile(p)               => s"add-file $p"
    case Command.AddDirectory(p)          => s"add-directory $p"
    case Command.RemoveSource(p)          => s"remove-source $p"
    case Command.AddMedia(p)              => s"add-media $p"
    case Command.RemoveMedia(p)           => s"remove-media $p"
    case Command.MoveMedia(a, b)          => s"move-media $a $b"
    case Command.LimitMedia(p, n)         => s"limit-media $p $n"
    case Command.LabelMedia(p, l)         => s"label-media $p $l"
    case Command.ConnectMedia(n, p)       => s"connect-media $n $p"
    case Command.OpenManifest(p)          => s"open-manifest $p"
    case Command.Restore(t)               => s"restore $t"
    case other                            => other.toString
