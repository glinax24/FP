package transfer.operations

import transfer.domain.*
import transfer.util.Fs

import java.nio.file.{Files, InvalidPathException, Path, Paths}
import scala.jdk.CollectionConverters.*

//собирает выбранные файлы в переносимый набор
object Collector:

  //разрешает конфликты одинаковых относительных путей, оставляя первый файл и исключая остальные
  def resolveConflicts(entries: List[SourceEntry], conflicts: List[String]): (List[SourceEntry], List[String]) =
    if conflicts.isEmpty then (entries, Nil)
    else
      val kept = scala.collection.mutable.Set.empty[String]
      val dropped = scala.collection.mutable.ListBuffer.empty[String]
      val result = entries.filter { entry =>
        if !entry.isFile || !conflicts.contains(entry.relPathOf) then true
        else if kept.add(entry.relPathOf) then true
        else
          dropped += entry.absPathOf
          false
      }
      (result, dropped.toList)

  //собирает выбранные объекты в готовый TransferSet: файлы, папки, размеры и найденные проблемы
  def collect(id: String, name: String, items: List[SelectedItem]): Either[AppError, TransferSet] =
    for
      merged       <- merge(items.map(scan))
      entries       = dedupeByPath(merged)
      conflicts     = findPathConflicts(entries)
      (keep, dropped) = resolveConflicts(entries, conflicts)
      files = keep.collect { case f: SourceEntry.File => f }
      dirs  = keep.collect { case d: SourceEntry.Dir => d }
    yield
      TransferSet(
        id = id,
        name = name,
        createdAt = TransferSet.nowIso,
        entries = keep,
        totalSize = files.map(_.size).sum,
        fileCount = files.size,
        directoryCount = dirs.size,
        unreadable = files.filterNot(_.readable).map(_.absPath),
        pathConflicts = conflicts,
        dropped = dropped
      )

  //объединяет результаты сканирования нескольких объектов в один список
  private def merge(results: List[Either[AppError, List[SourceEntry]]]): Either[AppError, List[SourceEntry]] =
    results.collectFirst { case Left(err) => err } match
      case Some(err) => Left(err)
      case None      => Right(results.collect { case Right(xs) => xs }.flatten)

  //удаляет повторяющиеся объекты по абсолютному пути
  private def dedupeByPath(entries: List[SourceEntry]): List[SourceEntry] =
    val seen = scala.collection.mutable.Set.empty[String]
    entries.filter { entry =>
      val key =
        try Paths.get(entry.absPathOf).normalize().toString
        catch
          case _: InvalidPathException => entry.absPathOf
      seen.add(key)
    }


  //проверяет выбранный объект и сканирует файл или директорию
  private def scan(sel: SelectedItem): Either[AppError, List[SourceEntry]] = sel match
    case SelectedItem.File(abs, rel) =>
      if !Fs.exists(abs) then Left(AppError.SourceNotFound(abs))
      else if !Fs.isRegularFile(abs) then Left(AppError.SourceNotFound(s"$abs (не файл)"))
      else
        for
          size <- Fs.size(abs)
          mod  <- Fs.modifiedIso(abs)
        yield List(SourceEntry.File(abs, Fs.normalizeRel(rel), size, mod, readable = Fs.isReadable(abs)))
    case SelectedItem.Directory(abs, rel) =>
      if !Fs.exists(abs) then Left(AppError.SourceNotFound(abs))
      else if !Fs.isDirectory(abs) then Left(AppError.SourceNotFound(s"$abs (не каталог)"))
      else scanDirectory(abs, Fs.normalizeRel(rel))


  //обходит директорию и собирает все найденные файлы и вложенные папки
  private def scanDirectory(abs: String, relPrefix: String): Either[AppError, List[SourceEntry]] =
    try
      val root = Paths.get(abs)
      val prefix = Paths.get(relPrefix)
      val stream = Files.walk(root)
      try
        val paths = stream.iterator.asScala.toList
        val dirs = paths
          .filter(p => p != root && Files.isDirectory(p))
          .map { p =>
            SourceEntry.Dir(p.toString, Fs.normalizeRel(prefix.resolve(root.relativize(p)).toString))
          }
        val files = paths
          .filter(Files.isRegularFile(_))
          .map { p =>
            val rel = Fs.normalizeRel(prefix.resolve(root.relativize(p)).toString)
            SourceEntry.File(p.toString, rel, Files.size(p), modifiedIso(p), Files.isReadable(p))
          }
        Right(dirs ::: files)
      finally stream.close()
    catch
      case e: Exception => Left(AppError.SourceUnreadable(s"$abs (${e.getMessage})"))


  //получает дату изменения файла в формате ISO
  private def modifiedIso(p: Path): String =
    java.time.Instant.ofEpochMilli(Files.getLastModifiedTime(p).toMillis).toString

  //находит файлы с одинаковыми относительными путями
  private def findPathConflicts(entries: List[SourceEntry]): List[String] =
    entries
      .collect { case f: SourceEntry.File => f }
      .groupBy(_.relPath)
      .collect { case (rel, fs) if fs.map(_.absPath).distinct.size > 1 => rel }
      .toList
      .sorted