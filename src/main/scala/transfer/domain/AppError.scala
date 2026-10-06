package transfer.domain

import transfer.util.SizeFormat

/** Ошибки приложения, объединённые в единый ADT */
enum AppError:
  case SourceNotFound(path: String)
  case SourceUnreadable(path: String)
  case PathConflict(relPath: String)
  case MediaUnavailable(path: String)
  case MediaNotWritable(path: String)
  case NotEnoughSpaceOnMedia(path: String, required: Long, available: Long)
  case NotEnoughTotalSpace(required: Long, available: Long)
  case WrongMedia(expected: String, actual: String)
  case MediaWrongNumber(expected: Int, actual: Int)
  case PartMissing(number: Int)
  case UnexpectedPart(what: String)
  case SizeMismatch(what: String, expected: Long, actual: Long)
  case ChecksumMismatch(what: String, expected: String, actual: String)
  case ManifestCorrupted(detail: String)
  case SetNotCompleted(name: String)
  case SourceChanged(path: String)
  case NotEnoughSpaceForRestore(required: Long, available: Long)
  case OperationInterrupted
  case Suspended(detail: String)
  case InvalidConfig(detail: String)
  case UnknownCommand(line: String)
  case MissingState(detail: String)
  case NotImplemented(what: String)

  // Понятное пользователю сообщение об ошибке
  def message: String = this match
    case SourceNotFound(p)      => s"Исходный файл не найден: $p"
    case SourceUnreadable(p)    => s"Не удалось прочитать: $p"
    case PathConflict(r)        => s"Конфликт относительных путей: $r"
    case MediaUnavailable(p)    => s"Носитель недоступен: $p"
    case MediaNotWritable(p)    => s"Носитель защищён от записи: $p"
    case NotEnoughSpaceOnMedia(p, req, av) =>
      s"Недостаточно места на носителе $p: требуется ${SizeFormat.humanReadable(req)}, доступно ${SizeFormat.humanReadable(av)}"
    case NotEnoughTotalSpace(req, av) =>
      s"Суммарного объёма носителей недостаточно: требуется ${SizeFormat.humanReadable(req)}, доступно ${SizeFormat.humanReadable(av)}"
    case WrongMedia(expected, actual) =>
      s"Подключён не тот носитель: ожидался $expected, фактически $actual"
    case MediaWrongNumber(expected, actual) =>
      s"Носитель с неправильным номером: ожидался $expected, фактически $actual"
    case PartMissing(n)         => s"Часть $n отсутствует"
    case UnexpectedPart(w)      => s"Обнаружена лишняя часть: $w"
    case SizeMismatch(w, e, a)  => s"Не совпадает размер $w: ожидалось ${SizeFormat.humanReadable(e)}, фактически ${SizeFormat.humanReadable(a)}"
    case ChecksumMismatch(w, e, a) => s"Контрольная сумма не совпадает ($w): ожидалось $e, фактически $a"
    case ManifestCorrupted(d)   => s"Манифест повреждён: $d"
    case SetNotCompleted(n)     => s"Набор не завершён: $n"
    case SourceChanged(p)       => s"Исходные данные изменились после планирования: $p"
    case NotEnoughSpaceForRestore(req, av) =>
      s"Недостаточно места для восстановления: требуется ${SizeFormat.humanReadable(req)}, доступно ${SizeFormat.humanReadable(av)}"
    case OperationInterrupted   => "Операция была прервана"
    case Suspended(detail)      => s"Запись приостановлена.\n\n$detail"
    case InvalidConfig(d)       => s"Файл конфигурации не найден: $d"
    case UnknownCommand(l)      => s"Неизвестная команда: $l"
    case MissingState(d)        => s"Недостаточно данных: $d"
    case NotImplemented(w)      => s"Не реализовано: $w"