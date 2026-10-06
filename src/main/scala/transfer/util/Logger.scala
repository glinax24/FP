package transfer.util

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter

// Простое журналирование: консоль и, при необходимости, файл 
object Logger:
  //запись в консоль
  def info(msg: String): Unit = println(s"[info]  $msg")
  def warn(msg: String): Unit = println(s"[warn]  $msg")
  def error(msg: String): Unit = println(s"[error] $msg")
  //запись в консоль и в файл
  def info(msg: String, logFile: String): Unit = { info(msg); append(logFile, msg) }
  def warn(msg: String, logFile: String): Unit = { warn(msg); append(logFile, msg) }
  def error(msg: String, logFile: String): Unit = { error(msg); append(logFile, msg) }

  private def append(logFile: String, msg: String): Unit =
    try
      val path = Paths.get(logFile)//получаем путь лога
      if path.getParent != null then Files.createDirectories(path.getParent)//смотрим есть ли в пути лога какие-то папки. если папки еще не созданы, то программа их создаст
      val line =
        s"${LocalDateTime.now().format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)}  $msg${System.lineSeparator()}"//время+текст самого соо+переход на новую строку
      Files.write(path, line.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.APPEND)//StandardOpenOption.CREATE — если самого лога еще нет прога создастновый пустой файл.StandardOpenOption.APPEND — если лог уже есть и там много записей,  не стирай старое,просто открой самую последнюю страницу и допиши нашу новую строчку в самый конец
    catch
      case _: Exception => ()