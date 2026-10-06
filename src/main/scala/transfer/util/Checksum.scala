package transfer.util

import transfer.domain.AppError

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import java.security.MessageDigest//умеет вычислять контрольные суммы с помощью алгоритмов
import scala.util.control.NonFatal//для обработки обычных ошибок, которые могут возникнуть во время работы с файлом

//Подсчёт контрольных сумм  
object Checksum:

  def ofBytes(bytes: Array[Byte], algorithm: String = "SHA-256"): String =
    hex(MessageDigest.getInstance(algorithm).digest(bytes))//Генерирует уникальный Hex-хэш (цифровой отпечаток) из массива байтов

  def ofString(s: String, algorithm: String = "SHA-256"): String =//
    ofBytes(s.getBytes(StandardCharsets.UTF_8), algorithm)//Хеширует строку в UTF-8 и возвращает результат по умолчанию в SHA-256

  // Контрольная сумма файла в нижнем регистре, шестнадцатеричная
  def ofFile(path: Path, algorithm: String = "SHA-256"): Either[AppError, String] =
    try
      val digest = MessageDigest.getInstance(algorithm)//объект, который будет накапливать хэш
      val in = Files.newInputStream(path)//открытие файла
      try
        val buffer = new Array[Byte](64 * 1024)//массив 64 кб, временное место куда программа будет загружать очередной кусок файла
        var read = in.read(buffer)//читаем первуб порцию данных и в буфер их. а в переменной read хранится сколько байт нам удалось прочитать
        while read >= 0 do//цикл крутится пока файл не кончится
          if read > 0 then digest.update(buffer, 0, read)//возьми данные из буфера, начни с самого начала(0) и возьми столько байт сколько успешно прочитал read
          read = in.read(buffer)//берем новую порцию данных и в буфер мх
        Right(hex(digest.digest()))//готовый хэш
      finally in.close()
    catch
      case NonFatal(e) => Left(AppError.SourceUnreadable(s"$path (${e.getMessage})"))

  //Завершает накопительный подсчёт и возвращает контрольную сумму в нижнем регистре
  def finish(digest: MessageDigest): String = hex(digest.digest())

  //Переводит каждый байт в 2 символа Hex (с ведущими нулями, нижний регистр) и склеивает их в одну строку
  private def hex(bytes: Array[Byte]): String =
    bytes.map(b => f"$b%02x").mkString