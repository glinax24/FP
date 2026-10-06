import transfer.cli.Repl
import transfer.config.ConfigLoader
import transfer.util.Logger

import java.nio.file.Paths
// [Console]::OutputEncoding = [System.Text.Encoding]::UTF8
@main
def transferManager(): Unit =
  val configPath = "transfer.json"
  ConfigLoader.resolve(Paths.get(configPath)) match  //создаем объект пути к конфиг файлу и после пытаемся найти и загрузить конфигурацию из этого файла
    case Right(config) => //в этой переменной находится сама конфигурация приложения
      Logger.info("Приложение запущено")//в журнал записывается это
      Repl.run(config, configPath) //Repl.run() запускает консольный режим программы, в котором пользователь вводит команды, а программа их обрабатывает
    case Left(err) =>
      Logger.error(err.message) //в журнал записывается соо об ошибке