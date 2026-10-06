package transfer.domain

import transfer.util.Fs

import java.nio.file.Paths

//Media описывает носители и операции для получения и обновления информации о них

//хранит информацию об одном носителе: 
//его название, путь, общий и свободный объём, возможность записи и установленное вручную ограничение по размеру
case class Media(
    label: String,
    path: String,
    totalSpace: Long,
    availableSpace: Long,
    writable: Boolean,
    manualLimit: Option[Long] = None
)

object Media:

  //рассчитывает, сколько места на носителе можно использовать для записи
  def usableSpace(media: Media, reserved: Long): Long =
    media.manualLimit.getOrElse(Math.max(0L, media.availableSpace - reserved))

 //обновляет информацию о носителе: свободное место, общий объём и доступность для записи
  def refresh(media: Media): Media =
    if Fs.isDirectory(media.path) then
      val root = Paths.get(media.path)
      media.copy(totalSpace = Fs.totalSpace(root), availableSpace = Fs.availableSpace(root), writable = Fs.isWritable(media.path))
    else media.copy(availableSpace = 0L, writable = false)

  //подключает носитель по новому пути и обновляет его информацию
  def connectedTo(media: Media, path: String): Media =
    refresh(media.copy(path = path))