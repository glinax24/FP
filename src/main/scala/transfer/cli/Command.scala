package transfer.cli

//Команды текстового интерфейса 
enum Command: //enum — это тип, у которого есть заранее перечисленные варианты
  case Help
  case CreateSet(name: String)
  case AddFile(path: String)
  case AddDirectory(path: String)
  case RemoveSource(path: String)
  case ShowSources
  case AddMedia(path: String)
  case RemoveMedia(path: String)
  case MoveMedia(from: Int, to: Int)
  case RescanMedia
  case LimitMedia(path: String, limit: Long)
  case LabelMedia(path: String, label: String)
  case ConnectMedia(number: Int, path: String)
  case ShowMedia
  case BuildPlan
  case ConfirmWrite
  case Write
  case Continue
  case CancelWrite
  case Rebuild
  case ExcludeUnreadable
  case RecheckSources
  case ExcludeChanged
  case Verify
  case Restore(target: String)
  case OpenManifest(path: String)
  case ShowManifest
  case ShowReport
  case Reload
  case Exit

object Command: //Repl
  def usage: String =
    """Команды:
      |  create-set <имя>       создать новый переносимый набор
      |  add-file <путь>        добавить файл
      |  add-directory <путь>   добавить каталог с вложенным содержимым
      |  remove-source <путь>   исключить ранее добавленный файл или каталог
      |  sources                показать состав выбранных данных
      |  add-media <путь>       добавить носитель (каталог)
      |  remove-media <путь>    удалить носитель из списка
      |  move-media <от> <до>   изменить порядок носителей (позиции с 1)
      |  rescan-media           заново определить доступный объём носителей
      |  limit-media <путь> <размер>  ограничить используемый объём носителя
      |  label-media <путь> <название>  задать название носителя
      |  connect-media <номер> [<путь>]  подключить носитель под указанным номером
      |  media                  показать носители и их порядок
      |  plan                   построить план распределения
      |  write-confirm          подтвердить план и начать запись
      |  write                  начать запись (требует подтверждения плана)
      |  continue               продолжить прерванную запись
      |  cancel-write           отменить прерванную запись
      |  rebuild                заново сформировать набор
      |  exclude-unreadable     исключить нечитаемые файлы из набора
      |  recheck-sources        повторить проверку исходных файлов
      |  exclude-changed        исключить изменившиеся с момента формирования набора файлы
      |  verify                 проверить записанный набор
      |  restore <каталог>      восстановить исходные файлы
      |  open-manifest <путь>   загрузить манифест из файла
      |  manifest               показать информацию из манифеста
      |  report                 показать итоговый сводный отчёт
      |  reload                 перезагрузить конфигурацию
      |  exit                   завершить работу""".stripMargin