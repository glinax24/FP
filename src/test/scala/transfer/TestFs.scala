package transfer

import java.io.ByteArrayOutputStream
import java.nio.file.{Files, Path}
import scala.jdk.CollectionConverters.*

/** Общие помощники для тестов, работающих с временными каталогами. */
object TestFs:

  def deleteTree(dir: Path): Unit =
    val children = Files.list(dir).iterator.asScala.toList
    children.foreach { child =>
      if Files.isDirectory(child) then deleteTree(child) else Files.deleteIfExists(child)
    }
    Files.deleteIfExists(dir)

  /** Создаёт файл с заданным содержимым, создавая промежуточные каталоги. */
  def createFile(path: Path, content: Array[Byte]): Path =
    Option(path.getParent).foreach(Files.createDirectories(_))
    Files.write(path, content)
    path

  def readAll(path: Path): Array[Byte] = Files.readAllBytes(path)

  /** Детерминированное содержимое файла заданного размера. */
  def pattern(size: Int): Array[Byte] = Array.tabulate[Byte](size)(i => ((i * 31 + 7) % 251).toByte)

  /** Склейка содержимого файлов в указанном порядке — так данные собираются обратно при восстановлении. */
  def concat(paths: List[Path]): Array[Byte] =
    val out = new ByteArrayOutputStream()
    paths.foreach(p => out.write(readAll(p)))
    out.toByteArray
