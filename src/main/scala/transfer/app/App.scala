package transfer.app

import transfer.cli.Command
import transfer.config.ConfigLoader
import transfer.domain.*
import transfer.operations.*
import transfer.planning.{Plan, Planner}
import transfer.util.{Fs, Logger, SizeFormat}

import java.nio.file.{Files, Paths}

//App принимает команды пользователя и решает, 
//какой компонент программы должен выполнить нужное действие
object App:

  //создаем начальное состояние программы
  def empty(config: transfer.config.Config, configPath: String): AppState =
    AppState(config, configPath, None, Nil, Nil, None, None) 

  def apply(cmd: Command, state: AppState): Either[AppError, (AppState, String)] =
    apply(cmd, state, null, None)

  //главная функция обработки команд в App
  //определяет, какая именно команда пришла, и выполняет соответствующее действие
  def apply(
      cmd: Command,
      state: AppState,
      confirm: String => Boolean,//перезаписывать существующий файл?
      request: Option[MediaRequest]//способ получения пути к очередному носителю
  ): Either[AppError, (AppState, String)] = cmd match
    
    //Кейсы в App обрабатывают команды пользователя: проверяют необходимые условия, 
    //вызывают соответствующий компонент и формируют новое состояние приложения
    case Command.Help =>
      Right((state, Command.usage))

    //нет ли уже сохранённой незавершённой операции в рабочем каталоге. да: восст инф. нет:создать нов набор
    case Command.CreateSet(name) =>
      SessionIO.load(Paths.get(state.config.workDirectory)) match // SessionIO.load(...) пытается загрузить сохранённое состояние предыдущей операции
        case Right(Some(session)) =>
          val fresh = TransferSet(session.setId, session.setName, TransferSet.nowIso, Nil, 0L, 0, 0, Nil, Nil)//TransferSet-инф о переносимом наборе файлов
          Logger.info(s"Создан набор «${fresh.name}» (идентификатор ${fresh.id}); обнаружена незавершённая операция", state.config.logFile)
          val freshStateValue = freshState(fresh, state)//новое состояние
          val message =
            s"Обнаружена незавершённая операция:\n\n" +
            s"Набор: ${session.setName}\n" +
            s"Завершено носителей: ${session.completedMedia} из ${session.totalMedia}\n" +
            s"Последняя проверенная часть: ${session.verifiedParts.lastOption.getOrElse(0)}\n" +
            s"Следующая часть: ${session.nextPart.map(_.toString).getOrElse("-")}\n\n" +
            s"Добавьте те же исходные файлы и каталоги, затем выполните continue."
          Right((freshStateValue, message + "\n\n" + Report.finalReport(freshStateValue)))
        case _ => //просто создае новый набор
          val fresh = TransferSet(TransferSet.freshId, name, TransferSet.nowIso, Nil, 0L, 0, 0, Nil, Nil)
          Logger.info(s"Создан набор «$name» (идентификатор ${fresh.id})", state.config.logFile)
          val created = freshState(fresh, state)//новое состояние
          Right(
            (created,
            s"Создан новый набор: $name (идентификатор ${fresh.id}). Добавьте файлы и каталоги." +
              "\n\n" + Report.finalReport(created))
          )
    //добавляет файл в список источников и пересобирает информацию о наборе
    case Command.AddFile(path) =>
      for
        newSelected <- addSelection(SelectedItem.File(path, Fs.baseName(path)), state)//если файл сущ то добавляем его в selected
        result      <- rebuildSet(state.copy(selected = newSelected))//пересобираем инф о наборе файлов. получаем новое состояние и соо пользователю
      yield
        Logger.info(s"Добавлен исходный файл: $path", state.config.logFile)
        result

    //добавляет каталог в список источников и пересобирает набор
    case Command.AddDirectory(path) =>
      for
        newSelected <- addSelection(SelectedItem.Directory(path, Fs.baseName(path)), state)
        result      <- rebuildSet(state.copy(selected = newSelected))
      yield
        Logger.info(s"Добавлен исходный каталог: $path", state.config.logFile)
        result
    //удаляет файл/каталог из источников и пересобирает набор
    case Command.RemoveSource(path) =>
      val remaining = state.selected.filterNot(_.absPathOf == path)
      if remaining.size == state.selected.size then
        Left(AppError.MissingState(s"источник не найден в наборе: $path"))
      else
        Logger.info(s"Исключён исходный объект: $path", state.config.logFile)
        rebuildSet(state.copy(selected = remaining))

    //показывает текущие выбранные файлы и каталоги
    case Command.ShowSources =>
      requireSet(state).map { set =>
        (state, Report.setSummary(set) + triageHint(state))
      }
    
    //добавляет носитель
    case Command.AddMedia(path) =>
      addMedia(path, state)

    //удаляет носитель
    case Command.RemoveMedia(path) =>
      val remaining = state.media.filterNot(_.path == path)
      if remaining.size == state.media.size then Left(AppError.MissingState(s"носитель не найден: $path"))
      else
        Logger.info(s"Носитель удалён из списка: $path", state.config.logFile)
        Right(
          (state.copy(media = remaining, plan = None, planConfirmed = false),
          s"Носитель удалён из списка: $path")
        )

    //меняет порядок носителей
    case Command.MoveMedia(from, to) =>
      val count = state.media.size
      if count == 0 then Left(AppError.MissingState("носители не добавлены — выполните add-media <путь>"))
      else if from < 1 || to < 1 || from > count || to > count then
        Left(AppError.MissingState(s"позиция за пределами списка (всего $count носителей)"))
      else if from != to then
        val moved = state.media.updated(from - 1, state.media(to - 1)).updated(to - 1, state.media(from - 1))
        Logger.info(s"Изменён порядок носителей: позиции $from и $to", state.config.logFile)
        Right(
          (state.copy(media = moved, plan = None, planConfirmed = false),
          s"Порядок изменён: позиции $from и $to поменялись местами.\n" + Report.mediaList(moved, state.config.reservedSpacePerMedia))
        )
      else Right((state, "Носители не перемещены (позиции совпадают)."))

    //заново проверяет свободное место на носителях
    case Command.RescanMedia =>
      val refreshed = state.media.map(Media.refresh)
      Logger.info("Носители перепроверены: доступный объём определён заново", state.config.logFile)
      Right(
        (state.copy(media = refreshed, plan = None, planConfirmed = false),
        Report.mediaList(refreshed, state.config.reservedSpacePerMedia))
      )

    //задаёт вручную ограничение объёма носителя
    case Command.LimitMedia(path, limit) =>
      val index = state.media.indexWhere(_.path == path)
      if index < 0 then Left(AppError.MissingState(s"носитель не найден: $path"))
      else if limit < 0 then Left(AppError.InvalidConfig("ограничение не может быть отрицательным"))
      else
        val updated = state.media.updated(index, state.media(index).copy(manualLimit = Some(limit)))
        Logger.info(s"Носитель ${state.media(index).label}: разрешённый объём ограничен ${SizeFormat.humanReadable(limit)}", state.config.logFile)
        Right(
          (state.copy(media = updated, plan = None, planConfirmed = false),
          s"Для носителя ${updated(index).label} разрешено использовать: ${SizeFormat.humanReadable(Media.usableSpace(updated(index), state.config.reservedSpacePerMedia))}")
        )

    //задаёт название носителю
    case Command.LabelMedia(path, label) =>
      val index = state.media.indexWhere(_.path == path)
      if index < 0 then Left(AppError.MissingState(s"носитель не найден: $path"))
      else if label.trim.isEmpty then Left(AppError.MissingState("название носителя не может быть пустым"))
      else
        val updated = state.media.updated(index, state.media(index).copy(label = label.trim))
        Logger.info(s"Носителю $path присвоено название ${label.trim}", state.config.logFile)
        Right((state.copy(media = updated), s"Носитель $path переименован: ${updated(index).label}"))

    //подключить носитель под указанным номером
    case Command.ConnectMedia(number, path) =>
      connectMedia(number, path, state)

    //инф о носителях
    case Command.ShowMedia =>
      Right((state, Report.mediaList(state.media, state.config.reservedSpacePerMedia)))

    // создаёт план: как распределить файлы/части по носителям
    case Command.BuildPlan =>
      for
        set  <- requireSet(state)
        st2  <- requireMedia(state)
        st3  <- requireTriage(st2)
        plan <- Planner.buildPlan(set, st2.media, st2.config)
        _     = Logger.info(
          s"Построен план набора «${plan.name}»: частей ${plan.parts.size}, носителей ${plan.allocations.size}, " +
          s"размер части ${SizeFormat.humanReadable(plan.partSize)}",
          st2.config.logFile
        )
      yield
        val next = st2.copy(
          plan = Some(plan),
          planConfirmed = false,
          manifest = None,
          manifestPath = Some(workManifestPath(st2).toString)
        )
        (next, Report.planSummary(plan) + "\n\nПлан построен. Для начала записи подтвердите его: write-confirm")

    //подтверждает, что план можно выполнять
    case Command.ConfirmWrite =>
      if state.plan.isEmpty then Left(AppError.MissingState("план не построен — выполните plan"))
      else
        Logger.info("План подтверждён пользователем", state.config.logFile)
        Right((state.copy(planConfirmed = true), "План подтверждён. Начинайте запись: write"))

    //выполняет запись файлов по плану
    case Command.Write =>
      for
        set      <- requireSet(state)
        st2      <- requireMedia(state)
        st3      <- requirePlan(st2)
        st4      <- requirePlanConfirmed(st2)
        progress <- Writer.writeSet(set, st3, st4.media, st4.config, request)
      yield
        val next = afterWrite(st4, progress)
        (next, Report.writeSummary(progress) + "\n" + Report.finalReport(next))

    //продолжает прерванную запись
    case Command.Continue =>
      for
        st2      <- requireMedia(state)
        _        <- requireSet(state)
        workMan  <- if st2.plan.isEmpty then workManifest(st2).map(Option(_)) else Right(st2.manifest)
        set      <- requireSet(st2)
        plan     <- resolvePlan(st2, workMan)
        progress <- Writer.resume(set, plan, st2.media, st2.config, request)
      yield
        val next = afterWrite(st2, progress)
        (next, Report.writeSummary(progress) + "\n" + Report.finalReport(next))

    //отменяет текущую незавершённую операцию
    case Command.CancelWrite =>
      SessionIO.clear(Paths.get(state.config.workDirectory)).map { _ =>
        Logger.info("Прерванная запись отменена, сохранённое состояние удалено", state.config.logFile)
        (
          state.copy(manifest = None, plan = None, planConfirmed = false, manifestPath = None),
          "Прерванная запись отменена, состояние очищено."
        )
      }

    //полностью пересоздаёт набор и начинает планирование заново
    case Command.Rebuild =>
      val cleared = SessionIO.clear(Paths.get(state.config.workDirectory))
      val fresh = TransferSet(
        TransferSet.freshId,
        state.set.map(_.name).getOrElse("набор"),
        TransferSet.nowIso,
        Nil,
        0L,
        0,
        0,
        Nil,
        Nil
      )
      cleared.map { _ =>
        Logger.info(s"Набор сформирован заново: ${fresh.name} (идентификатор ${fresh.id})", state.config.logFile)
        (
          freshState(fresh, state),
          s"Набор сформирован заново: ${fresh.name} (идентификатор ${fresh.id}). Добавьте файлы и каталоги."
        )
      }

    //исключает нечитаемые файлы из набора
    case Command.ExcludeUnreadable =>
      state.set match
        case None => Left(AppError.MissingState("набор не создан — выполните create-set <имя>"))
        case Some(set) if set.unreadable.isEmpty =>
          Right((state.copy(pendingUnreadable = false), "Нечитаемых файлов в наборе нет."))
        case Some(set) =>
          val excluded = set.unreadable
          val keep = set.entries.filterNot(e => e.isFile && excluded.contains(e.absPathOf))
          val next = withEntries(state, set, keep, s"Исключено нечитаемых файлов: ${excluded.size}")
          Right((next, Report.setSummary(next.set.get) + triageHint(next)))

    //повторно проверяет исходные файлы
    case Command.RecheckSources =>
      state.set match
        case None => Left(AppError.MissingState("набор не создан — выполните create-set <имя>"))
        case Some(set) =>
          Collector.collect(set.id, set.name, state.selected).map { collected =>
            Logger.info(s"Состав набора «${set.name}» проверен заново: файлов ${collected.fileCount}", state.config.logFile)
            val next = collectedState(state, collected)
            (next, Report.setSummary(collected) + triageHint(next))
          }

    //исключает файлы, которые изменились после добавления.
    case Command.ExcludeChanged =>
      state.set match
        case None => Left(AppError.MissingState("набор не создан — выполните create-set <имя>"))
        case Some(set) =>
          val changed = set.entries.collect {
            case f: SourceEntry.File
                if !Fs.isRegularFile(f.absPath) || Fs.size(f.absPath).getOrElse(-1L) != f.size ||
                  Fs.modifiedIso(f.absPath).getOrElse("") != f.modifiedAt =>
              f.absPath
          }
          if changed.isEmpty then
            Right((state, "Изменившихся исходных файлов нет."))
          else
            val keep = set.entries.filterNot(e => e.isFile && changed.contains(e.absPathOf))
            val next = withEntries(state, set, keep, s"Исключено изменившихся файлов: ${changed.size}")
            Right(
              (
                next,
                s"Исключено изменившихся файлов: ${changed.size}. План сброшен — постройте его заново: plan\n" +
                Report.setSummary(next.set.get) + triageHint(next)
              )
            )

    //проверяет записанные данные по контрольным суммам
    case Command.Verify =>
      for
        manifest <- requireManifest(state)
        _        <- mediaCountMatches(manifest, state.media)
        report   <- Verifier.verify(manifest, state.media.map(m => Paths.get(m.path)), state.config, request)
      yield
        val failures = report.results.flatMap(_.failures).map(_.message)
        val next = state.copy(verifyReport = Some(report), errors = failures)
        (next, Report.verifySummary(report))

    //восстанавливает файлы из распределённых частей
    case Command.Restore(target) =>
      for
        manifest <- requireManifest(state)
        _        <- mediaCountMatches(manifest, state.media)
        pre       = Report.restorePreSummary(manifest, target)
        roots     = state.media.map(m => Paths.get(m.path))
        report   <- if confirm == null then Restorer.restore(manifest, roots, Paths.get(target), state.config, request = request)
                    else Restorer.restore(manifest, roots, Paths.get(target), state.config, confirm, request)
      yield
        val next = state.copy(errors = report.problems.map(p => s"${p.relPath}: ${p.reason}"))
        (next, s"$pre\n${Report.restoreSummary(report)}")

    //загружает manifest.json
    case Command.OpenManifest(path) =>
      ManifestIO.readManifest(Paths.get(path)).map { m =>
        Logger.info(s"Загружен манифест набора «${m.name}» из $path", state.config.logFile)
        (state.copy(manifest = Some(m), manifestPath = Some(path)), s"Манифест загружен: $path\n" + Report.manifestSummary(m))
      }

    //показывает загруженный manifest
    case Command.ShowManifest =>
      requireManifest(state).map(m => (state, Report.manifestSummary(m)))

    //показывает итоговый отчёт
    case Command.ShowReport =>
      Right((state, Report.finalReport(state)))

    //заново загружает конфигурацию
    case Command.Reload =>
      ConfigLoader.reload(Paths.get(state.configPath)) match
        case Right(config) =>
          Logger.info(s"Конфигурация перезагружена: рабочий каталог ${config.workDirectory}, размер части ${SizeFormat.humanReadable(config.defaultPartSize)}", config.logFile)
          Right((state.copy(config = config), s"Конфигурация перезагружена.\n${Report.configSummary(config)}"))
        case Left(err) => Left(err)

    //сообщает о завершении работы приложения
    case Command.Exit =>
      Right((state, "Завершение работы."))

  //формирует подсказку пользователю, что делать с нечитаемыми файлами или конфликтами путей
  private def triageHint(s: AppState): String =
    if !s.pendingUnreadable && !s.pendingConflicts then ""
    else
      val lines =
        (if s.pendingUnreadable then List("Есть нечитаемые файлы. Выберите действие:\n  exclude-unreadable — исключить их\n  recheck-sources — повторить проверку\n  rebuild — отменить формирование набора") else Nil) ++
        (if s.pendingConflicts then List("Есть конфликты относительных путей: один файл исключён для каждого имени. Выберите действие:\n  remove-source — убрать лишний объект из набора\n  recheck-sources — повторить проверку\n  rebuild — отменить формирование набора") else Nil)
      "\n" + lines.mkString("\n")

  //создаёт новое чистое состояние для нового TransferSet: 
  //сбрасывает выбранные файлы, план, manifest и ошибки, но сохраняет конфигурацию и носители
  private def freshState(fresh: TransferSet, s: AppState): AppState =
    s.copy(
      set = Some(fresh),
      selected = Nil,
      plan = None,
      manifest = None,
      planConfirmed = false,
      pendingUnreadable = false,
      pendingConflicts = false,
      verifyReport = None,
      manifestPath = None
    )

  //проверяет, создан ли набор. Если да - возвращает его, если нет - ошибку
  private def requireSet(s: AppState): Either[AppError, TransferSet] =
    s.set match
      case Some(set) => Right(set)
      case None      => Left(AppError.MissingState("набор не создан — выполните create-set <имя>"))

  //проверяет, добавлены ли носители. Если список пуст - ошибка
  private def requireMedia(s: AppState): Either[AppError, AppState] =
    if s.media.isEmpty then Left(AppError.MissingState("носители не добавлены — выполните add-media <путь>"))
    else Right(s)

  //проверяет, построен ли план. Если нет - ошибка
  private def requirePlan(s: AppState): Either[AppError, Plan] =
    s.plan match
      case Some(p) => Right(p)
      case None    => Left(AppError.MissingState("план не построен — выполните plan"))

  //проверяет, подтверждён ли план перед записью
  private def requirePlanConfirmed(s: AppState): Either[AppError, AppState] =
    if s.planConfirmed then Right(s)
    else Left(AppError.MissingState("план не подтверждён — выполните plan, затем write-confirm"))

  //проверяет, решены ли проблемы с нечитаемыми файлами и конфликтами. Пока они есть - нельзя строить план
  private def requireTriage(s: AppState): Either[AppError, AppState] =
    if s.pendingUnreadable then
      Left(AppError.MissingState("в наборе есть нечитаемые файлы — выполните exclude-unreadable, recheck-sources или rebuild"))
    else if s.pendingConflicts then
      Left(AppError.MissingState("в наборе есть конфликты относительных путей — выполните remove-source, recheck-sources или rebuild"))
    else Right(s)

  //проверяет, есть ли manifest в состоянии
  private def requireManifest(s: AppState): Either[AppError, Manifest] =
    s.manifest match
      case Some(m) => Right(m)
      case None    => Left(AppError.MissingState("манифест недоступен — выполните write или open-manifest <путь>"))

  //роверяет, совпадает ли количество подключённых носителей с количеством носителей в manifest
  private def mediaCountMatches(m: Manifest, media: List[Media]): Either[AppError, Unit] =
    val need = m.mediaCount
    if media.size != need then
      Left(AppError.MediaUnavailable(s"подключено носителей ${media.size}, требуется $need — добавьте недостающие носители командой add-media"))
    else Right(())

  //строит путь к manifest в рабочем каталоге приложения
  private def workManifestPath(s: AppState) = Paths.get(s.config.workDirectory, s.config.manifestFile)

  //пытается загрузить manifest незавершённой операции из рабочего каталога
  private def workManifest(s: AppState): Either[AppError, Manifest] =
    val path = workManifestPath(s)
    if Files.exists(path) then ManifestIO.readManifest(path)
    else Left(AppError.MissingState(s"в рабочем каталоге нет манифеста прерванной записи ($path) — выполните rebuild"))

  //обновляет состояние после записи: сохраняет manifest и актуальные пути носителей
  private def afterWrite(s: AppState, progress: WriteProgress): AppState =
    val manifest = progress.manifest
    val refreshed = s.media.zipWithIndex.map { (media, i) =>
      manifest.mediaPaths.lift(i) match
        case Some(path) if path.nonEmpty && path != media.path => Media.connectedTo(media, path)
        case _                                                => media
    }
    s.copy(
      manifest = Some(manifest),
      planConfirmed = false,
      media = refreshed,
      manifestPath = Some(progress.manifestPath)
    )

  //определяет, какой план использовать для продолжения: текущий или восстановленный из manifest
  private def resolvePlan(s: AppState, manifest: Option[Manifest]): Either[AppError, Plan] =
    s.plan match
      case Some(p) => Right(p)
      case None =>
        manifest match
          case Some(m) => Right(Planner.planFromManifest(m))
          case None    => Left(AppError.MissingState("план не построен и нет манифеста прерванной записи — выполните plan"))

  //проверяет, существует ли добавляемый файл/каталог, и добавляет его в список выбранных источников
  private def addSelection(sel: SelectedItem, s: AppState): Either[AppError, List[SelectedItem]] = sel match
    case SelectedItem.File(p, _) if !Fs.exists(p) || !Fs.isRegularFile(p) =>
      Left(AppError.SourceNotFound(p))
    case SelectedItem.Directory(p, _) if !Fs.exists(p) || !Fs.isDirectory(p) =>
      Left(AppError.SourceNotFound(p))
    case _ =>
      Right(s.selected :+ sel)

  //заново собирает TransferSet по текущим выбранным файлам через Collector
  private def rebuildSet(s: AppState): Either[AppError, (AppState, String)] =
    s.set match
      case None =>
        Right((s, "Набор ещё не создан. Выполните create-set <имя>."))
      case Some(set) =>
        Collector.collect(set.id, set.name, s.selected).map { collected =>
          val next = collectedState(s, collected)
          (next, Report.setSummary(collected) + triageHint(next))
        }

  //помещает собранный TransferSet в состояние и сбрасывает старый план, потому что набор изменился
  private def collectedState(s: AppState, collected: TransferSet): AppState =
    s.copy(
      set = Some(collected),
      plan = None,
      planConfirmed = false,
      pendingUnreadable = collected.unreadable.nonEmpty,
      pendingConflicts = collected.pathConflicts.nonEmpty
    )

  //после исключения файлов пересчитывает размер и количество файлов/каталогов и обновляет состояние
  private def withEntries(s: AppState, set: TransferSet, keep: List[SourceEntry], reason: String): AppState =
    val files = keep.collect { case f: SourceEntry.File => f }
    val dirs = keep.collect { case d: SourceEntry.Dir => d }
    val updated = set.copy(
      entries = keep,
      totalSize = files.map(_.size).sum,
      fileCount = files.size,
      directoryCount = dirs.size,
      unreadable = Nil,
      dropped = Nil
    )
    Logger.info(s"Набор «${set.name}»: $reason", s.config.logFile)
    collectedState(s, updated)

  //добавляет новый носитель в список носителей программы и проверяет его доступное место
  private def addMedia(p: String, s: AppState): Either[AppError, (AppState, String)] =
    if !Fs.isDirectory(p) then Left(AppError.MediaUnavailable(p))
    else
      val root = Paths.get(p)
      val media = Media(Fs.baseName(p), p, Fs.totalSpace(root), Fs.availableSpace(root), Fs.isWritable(p), None)
      Logger.info(
        s"Добавлен носитель ${media.label} ($p): доступно ${SizeFormat.humanReadable(media.availableSpace)}, позиция ${s.media.size + 1}",
        s.config.logFile
      )
      Right(
        (s.copy(media = s.media :+ media, plan = None, planConfirmed = false),
        s"Носитель добавлен: ${media.label} — доступно ${SizeFormat.humanReadable(media.availableSpace)}")
      )

  //подключить носитель под указанным номером по указанному пути
  private def connectMedia(number: Int, path: String, s: AppState): Either[AppError, (AppState, String)] =
    if number < 1 || number > s.media.size then
      Left(AppError.MissingState(s"носитель с номером $number не добавлен (всего ${s.media.size})"))
    else if path.trim.isEmpty then
      Left(AppError.MissingState("укажите путь к подключённому носителю: connect-media <номер> <путь>"))
    else if !Fs.isDirectory(path.trim) then
      Left(AppError.MediaUnavailable(s"носитель $number: каталог ${path.trim} недоступен"))
    else
      val planned = s.media(number - 1)
      val updated = Media.connectedTo(planned, path.trim)
      val next = s.copy(media = s.media.updated(number - 1, updated), plan = None, planConfirmed = false)
      Logger.info(s"Носитель $number (${planned.label}) подключён по пути ${updated.path}", s.config.logFile)
      Right(
        (next,
        s"Носитель $number (${planned.label}) подключён: ${updated.path}\n" +
        s"Доступно: ${SizeFormat.humanReadable(updated.availableSpace)}\n" +
        s"Разрешено использовать: ${SizeFormat.humanReadable(Media.usableSpace(updated, s.config.reservedSpacePerMedia))}")
      )
