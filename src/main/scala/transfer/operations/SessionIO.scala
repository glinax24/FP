package transfer.operations

import transfer.domain.AppError

import java.nio.file.{Files, Path, StandardOpenOption}
import scala.util.control.NonFatal

//Этот файл отвечает за сохранение и восстановление состояния незавершённой записи

//Хранит состояние незавершённой записи: какие носители и части уже записаны и проверены, 
//и с какой части продолжать
case class SessionState(
    setId: String,
    setName: String,
    completedMedia: Int,
    totalMedia: Int,
    verifiedParts: List[Int],
    nextPart: Option[Int]
)

// Хранение состояния незавершённой записи в рабочем каталоге приложения 
object SessionIO:


  val FileName = "session.json"

  //сохраняет состояние записи в session.json
  def save(state: SessionState, workDir: Path): Either[AppError, Unit] =
    try
      Files.createDirectories(workDir)
      Files.writeString(
        workDir.resolve(FileName),
        ujson.write(toJson(state), indent = 2),
        StandardOpenOption.CREATE,
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.WRITE
      )
      Right(())
    catch case NonFatal(e) => Left(AppError.MissingState(s"не удалось сохранить состояние записи: ${e.getMessage}"))

  //загружает сохранённое состояние из session.json
  def load(workDir: Path): Either[AppError, Option[SessionState]] =
    val file = workDir.resolve(FileName)
    if !Files.exists(file) then Right(None)
    else
      try Right(Some(fromJson(ujson.read(Files.readString(file)))))
      catch case NonFatal(e) => Left(AppError.MissingState(s"состояние записи повреждено: ${e.getMessage}"))

  //удаляет сохранённое состояние записи
  def clear(workDir: Path): Either[AppError, Unit] =
    val file = workDir.resolve(FileName)
    if Files.exists(file) then Files.delete(file)
    Right(())

  //преобразует SessionState в JSON
  def toJson(state: SessionState): ujson.Obj =
    ujson.Obj(
      "setId" -> state.setId,
      "setName" -> state.setName,
      "completedMedia" -> state.completedMedia.toDouble,
      "totalMedia" -> state.totalMedia.toDouble,
      "verifiedParts" -> ujson.Arr.from(state.verifiedParts.map(_.toDouble)),
      "nextPart" -> state.nextPart.map(_.toDouble).getOrElse(-1.0)
    )

  //преобразует JSON обратно в SessionState
  def fromJson(json: ujson.Value): SessionState =
    val o = json.obj
    val next = o("nextPart").num.toInt
    SessionState(
      setId = o("setId").str,
      setName = o("setName").str,
      completedMedia = o("completedMedia").num.toInt,
      totalMedia = o("totalMedia").num.toInt,
      verifiedParts = o("verifiedParts").arr.toList.map(_.num.toInt),
      nextPart = if next < 0 then None else Some(next)
    )