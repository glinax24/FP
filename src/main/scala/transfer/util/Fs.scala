package transfer.util

import transfer.domain.AppError

import java.nio.file.{Files, Path, Paths}
import java.time.Instant

//Низкоуровневые операции с файловой системой
object Fs:

  def toPath(p: String): Path = Paths.get(p)//Функция получает строку с путём и превращает её в объект Path

  def exists(p: String): Boolean = Files.exists(toPath(p))//проверка существования файла или папки
  def isDirectory(p: String): Boolean = Files.isDirectory(toPath(p))//является ли путь папкой
  def isRegularFile(p: String): Boolean = Files.isRegularFile(toPath(p))//является ли путь обычным файлом
  def isReadable(p: String): Boolean = Files.isReadable(toPath(p))//можно ли читать этот файл или папку
  def isWritable(p: String): Boolean = Files.isWritable(toPath(p))//можно ли записывать

  def totalSpace(p: Path): Long = p.toFile.getTotalSpace//объем у диска/носителя
  def availableSpace(p: Path): Long = p.toFile.getUsableSpace//сколько место доступно для записи

  def size(p: String): Either[AppError, Long] =//получение размера файла
    try Right(Files.size(toPath(p)))
    catch case e: Exception => Left(AppError.SourceUnreadable(p))

  def modifiedIso(p: String): Either[AppError, String] = //получает дату и время последнего изменения файла
    try Right(Instant.ofEpochMilli(Files.getLastModifiedTime(toPath(p)).toMillis).toString)
    catch case e: Exception => Left(AppError.SourceUnreadable(p))

  def baseName(p: String): String =
    Option(toPath(p).getFileName).map(_.toString).getOrElse(p)//по пути берет имя последнего эл-та пути. если none то возвращаем только путь 

  //Приводит относительный путь к виду, единому для всех платформ
  def normalizeRel(s: String): String = s.replace('\\', '/')

  def ensureDir(p: Path): Either[AppError, Path] =//создаем каталог если его еще нет
    try { Files.createDirectories(p); Right(p) }
    catch case e: Exception => Left(AppError.InvalidConfig(s"Не удалось создать каталог $p: ${e.getMessage}"))