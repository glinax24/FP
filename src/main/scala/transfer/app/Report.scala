package transfer.app

import transfer.domain.*
import transfer.operations.{RestoreReport, VerifyReport}
import transfer.planning.Plan
import transfer.util.SizeFormat

//Report выводит информацию о работе программы в удобном для пользователя виде
//информацию о наборе файлов, носителях, плане распределения, манифесте, записи, проверке и восстановлении
object Report:

  //формирует полный итоговый отчёт по всей операции: набор, файлы, носители, части, ошибки, предупреждения, проверку
  def finalReport(s: AppState): String =
    val sb = StringBuilder("Итоговый отчёт\n\n")

    // Имя и идентификатор набора.
    s.set match
      case Some(set) =>
        sb.append(s"Набор: ${set.name} (id ${set.id})\n")
        sb.append(s"Создан: ${set.createdAt}\n")
        sb.append(s"Файлов: ${set.fileCount}\n")
        sb.append(s"Вложенных каталогов: ${set.directoryCount}\n")
        sb.append(s"Общий объём: ${SizeFormat.humanReadable(set.totalSize)}\n")
      case None => sb.append("Набор не создан\n")

// Список исходных объектов набора
    s.set.filter(_.entries.nonEmpty).foreach { set =>
      sb.append(s"\nИсходные объекты: ${set.entries.size}\n")
      set.entries.foreach(e => sb.append(s"  ${e.relPathOf} — ${SizeFormat.humanReadable(e.sizeOf)}\n"))
    }

    // Носители, части и распределение. Разделы присутствуют всегда, даже до плана 
    sb.append(s"\nНосителей: ${s.media.size}\n")
    s.media.zipWithIndex.foreach { (media, i) =>
      sb.append(s"  ${i + 1}. ${media.label} (${media.path}) — ${SizeFormat.humanReadable(media.availableSpace)}\n")
    }
    val partCount = s.plan.map(_.parts.size).orElse(s.manifest.map(_.parts.size)).getOrElse(0)
    sb.append(s"Частей: $partCount\n")
    s.plan match
      case Some(plan) =>
        sb.append(s"Распределение частей: размер части ${SizeFormat.humanReadable(plan.partSize)}\n")
        plan.allocations.foreach { a =>
          val label = plan.mediaLabels.lift(a.mediaNumber - 1).getOrElse(s"носитель ${a.mediaNumber}")
          sb.append(s"  Носитель ${a.mediaNumber} ($label): части ${compactRange(a.partNumbers)}\n")
        }
      case None => sb.append("Распределение частей: план ещё не построен\n")

    s.manifest.foreach { m =>
      sb.append(s"\nНосителей по манифесту: ${m.mediaCount}\n")
      sb.append(s"Частей по манифесту: ${m.parts.size}\n")
    }
    sb.append(s"Путь к манифесту: ${s.manifestPath.getOrElse("будет создан при записи")}\n")
    sb.append(s"Признак завершённости: ${if s.manifest.exists(_.completed) then "набор завершён" else "набор не завершён"}\n")

    // Результаты проверки
    s.verifyReport.foreach { v =>
      sb.append(s"\nРезультаты проверки: ${if v.failed then s"ошибок — ${v.results.flatMap(_.failures).size}" else "успешно"}\n")
      v.results.filterNot(_.ok).foreach { r =>
        sb.append(s"  Носитель ${r.mediaNumber} (${r.label}):\n")
        r.failures.foreach(f => sb.append(failureDetail(f)))
      }
    }

    // Предупреждения
    val warnings =
      s.plan.map(_.warnings).getOrElse(Nil) ++
      s.set.map(set => if set.dropped.nonEmpty then List(s"исключено файлов из-за конфликтов путей: ${set.dropped.size}") else Nil).getOrElse(Nil) ++
      s.set.map(set => if set.unreadable.nonEmpty then List(s"нечитаемых файлов: ${set.unreadable.size}") else Nil).getOrElse(Nil)
    if warnings.nonEmpty then
      sb.append(s"\nПредупреждения: ${warnings.size}\n")
      warnings.foreach(w => sb.append(s"  $w\n"))

    // Ошибки
    if s.errors.nonEmpty then
      sb.append(s"\nОшибки: ${s.errors.size}\n")
      s.errors.take(20).foreach(e => sb.append(s"  $e\n"))
    sb.toString


  //показывает краткую информацию о наборе: количество файлов, размер, нечитаемые файлы и конфликты
  def setSummary(set: TransferSet): String =
    val sb = StringBuilder()
    sb.append(s"Набор: ${set.name} (id ${set.id})\n")
    sb.append(s"Файлов: ${set.fileCount}\n")
    sb.append(s"Вложенных каталогов: ${set.directoryCount}\n")
    sb.append(s"Общий объём: ${SizeFormat.humanReadable(set.totalSize)}\n")
    if set.unreadable.nonEmpty then
      sb.append(s"\nНедоступно для чтения: ${set.unreadable.size} файлов\n")
      set.unreadable.foreach(f => sb.append(s"  $f\n"))
    if set.pathConflicts.nonEmpty then
      sb.append(s"\nКонфликтов путей: ${set.pathConflicts.size}\n")
      set.pathConflicts.foreach(c => sb.append(s"  $c\n"))
    if set.dropped.nonEmpty then
      sb.append(s"\nИсключено из-за конфликтов путей: ${set.dropped.size} файлов\n")
      set.dropped.foreach(f => sb.append(s"  $f\n"))
    sb.toString

  //показывает основные настройки конфигурации
  def configSummary(c: transfer.config.Config): String =
    val sb = StringBuilder()
    sb.append(s"Рабочий каталог: ${c.workDirectory}\n")
    sb.append(s"Размер части по умолчанию: ${SizeFormat.humanReadable(c.defaultPartSize)}\n")
    sb.append(s"Минимальный размер части: ${SizeFormat.humanReadable(c.minimumPartSize)}\n")
    sb.append(s"Резерв на носитель: ${SizeFormat.humanReadable(c.reservedSpacePerMedia)}\n")
    sb.append(s"Шаблон имени части: ${c.fileNameTemplate}\n")
    sb.toString

  //показывает список носителей, их пути, свободное место, резерв и доступный объём
  def mediaList(media: List[Media], reserved: Long): String =
    if media.isEmpty then "Носители не добавлены. Используйте add-media <путь>."
    else
      val sb = StringBuilder("Носители:\n")
      media.zipWithIndex.foreach { (m, i) =>
        sb.append(s"${i + 1}. ${m.label}\n")
        sb.append(s"   Путь: ${m.path}\n")
        sb.append(s"   Доступно физически: ${SizeFormat.humanReadable(m.availableSpace)}\n")
        sb.append(s"   Резерв: ${SizeFormat.humanReadable(reserved)}\n")
        m.manualLimit.foreach(limit => sb.append(s"   Ограничение вручную: ${SizeFormat.humanReadable(limit)}\n"))
        sb.append(s"   Разрешено использовать: ${SizeFormat.humanReadable(Media.usableSpace(m, reserved))}\n")
      }
      sb.toString

  //показывает план распределения файлов по носителям: части, размеры, сколько будет записано и сколько останется
  def planSummary(plan: Plan): String =
    val sb = StringBuilder()
    sb.append(s"План набора ${plan.name}\n\n")
    sb.append(s"Исходный объём: ${SizeFormat.humanReadable(plan.totalSize)}\n")
    sb.append(s"Носителей: ${plan.allocations.size}\n")
    sb.append(s"Частей: ${plan.parts.size}\n")
    sb.append(s"Размер части: ${SizeFormat.humanReadable(plan.partSize)}\n")
    sb.append(s"Суммарный объём достаточен: да\n")
    plan.allocations.zipWithIndex.foreach { (a, i) =>
      val label = plan.mediaLabels.lift(i).getOrElse(s"носитель ${a.mediaNumber}")
      sb.append(s"\nНоситель ${a.mediaNumber}: $label\n")
      val partsDesc =
        if a.partNumbers.isEmpty then "Частей нет"
        else s"Части: ${compactRange(a.partNumbers)}"
      sb.append(s"  $partsDesc\n")
      sb.append(s"  Будет записано: ${SizeFormat.humanReadable(a.writtenBytes)}\n")
      sb.append(s"  Останется: ${SizeFormat.humanReadable(a.remainingBytes)}\n")
    }
    if plan.warnings.nonEmpty then plan.warnings.foreach(w => sb.append(s"\nПредупреждение: $w\n"))
    sb.toString

  //показывает информацию из manifest: набор, файлы, носители, части и завершённость
  def manifestSummary(m: Manifest): String =
    val sb = StringBuilder()
    sb.append(s"Набор: ${m.name} (id ${m.setId})\n")
    sb.append(s"Создан: ${m.createdAt}\n")
    sb.append(s"Файлов: ${m.fileCount}\n")
    sb.append(s"Общий объём: ${SizeFormat.humanReadable(m.totalSize)}\n")
    sb.append(s"Носителей: ${m.mediaLabels.size} (ожидается подключить: ${m.mediaCount})\n")
    sb.append(s"Частей: ${m.parts.size}\n")
    sb.append(s"Завершён: ${if m.completed then "да" else "нет"}\n")
    sb.toString

  //показывает результат записи: сколько носителей и частей обработано, какая следующая часть и завершена ли операция
  def writeSummary(progress: transfer.operations.WriteProgress): String =
    val sb = StringBuilder()
    sb.append(s"Запись набора (id ${progress.setId})\n")
    sb.append(s"Носителей завершено: ${progress.completedMedia} из ${progress.totalMedia}\n")
    sb.append(s"Проверено частей: ${progress.verifiedParts.size}\n")
    progress.lastVerifiedPart.foreach(p => sb.append(s"Последняя проверенная часть: $p\n"))
    progress.nextPart match
      case Some(n) => sb.append(s"Следующая часть: $n\n")
      case None    => sb.append("Все части записаны и проверены.\n")
    sb.append(s"Набор завершён: ${if progress.manifest.completed then "да" else "нет"}\n")
    sb.append(s"Манифест набора: ${progress.manifestPath}\n")
    if !progress.manifestOnFirstMedia then
      sb.append(
        "\nВНИМАНИЕ: манифест сохранён во временном каталоге, потому что носитель, который шёл первым, не был доступен.\n" +
        "Перенесите манифест на носитель с частями вручную.\n"
      )
    sb.toString

  //показывает результат проверки записанных данных и ошибки, если они есть
  def verifySummary(r: VerifyReport): String =
    val sb = StringBuilder()
    sb.append(s"Проверка набора ${r.setName}\n")
    r.results.foreach { res =>
      val status = if res.ok then "успешно" else "ошибка"
      sb.append(s"\nНоситель ${res.mediaNumber} (${res.label}): $status\n")
      if !res.ok then res.failures.foreach(f => sb.append(failureDetail(f)))
    }
    if r.failed then sb.append("\nВосстановление полного набора невозможно.\n")
    sb.toString

  //формирует подробное описание ошибки проверки: неправильный размер, контрольная сумма или отсутствующая часть
  private def failureDetail(err: AppError): String = err match
    case AppError.SizeMismatch(what, expected, actual) =>
      s"  Повреждена часть: $what\n" +
      s"  Ожидаемый размер: ${SizeFormat.humanReadable(expected)}\n" +
      s"  Фактический размер: ${SizeFormat.humanReadable(actual)}\n"
    case AppError.ChecksumMismatch(what, expected, actual) =>
      s"  Повреждена часть: $what\n" +
      s"  Ожидаемая контрольная сумма: $expected\n" +
      s"  Фактическая контрольная сумма: $actual\n"
    case AppError.PartMissing(number) => s"  Отсутствует часть: $number\n"
    case other                        => s"  ${other.message}\n"

  //оказывает что и куда будет восстановлено перед началом восстановления
  def restorePreSummary(manifest: Manifest, targetDir: String): String =
    val sb = StringBuilder()
    sb.append(s"Набор: ${manifest.name}\n")
    sb.append(s"Носителей: ${manifest.mediaLabels.size}\n")
    sb.append(s"Частей: ${manifest.parts.size}\n")
    sb.append(s"Исходных файлов: ${manifest.fileCount}\n")
    sb.append(s"Исходный объём: ${SizeFormat.humanReadable(manifest.totalSize)}\n")
    sb.append(s"Каталог восстановления: $targetDir\n")
    sb.toString

  //показывает результат восстановления: сколько файлов/байт восстановлено и какие не удалось
  def restoreSummary(r: RestoreReport): String =
    val sb = StringBuilder()
    sb.append(s"Набор: ${r.setName}\n")
    sb.append(s"Каталог восстановления: ${r.targetDir}\n")
    sb.append(s"Носителей: ${r.mediaCount}\n")
    sb.append(s"Частей: ${r.partCount}\n")
    sb.append(s"Восстановлено файлов: ${r.restoredFiles}\n")
    sb.append(s"Восстановлено байт: ${SizeFormat.humanReadable(r.restoredBytes)}\n")
    sb.append(s"Проверено файлов: ${r.verifiedFiles}\n")
    sb.append(s"Повреждено или пропущено: ${r.failedFiles}\n")
    sb.append(s"Итоговый статус: ${r.status}\n")
    if r.problems.nonEmpty then
      sb.append("\nНе удалось восстановить:\n")
      r.problems.foreach(p => sb.append(s"  ${p.relPath}: ${p.reason}\n"))
    sb.toString

  //красиво сокращает номера частей: 1, 2, 3, 7  пареобразуются в 1–3, 7
  private def compactRange(nums: List[Int]): String =
    def group(acc: List[List[Int]], n: Int): List[List[Int]] =
      acc match
        case last :: rest if n - last.last == 1 => (last :+ n) :: rest
        case _ => List(n) :: acc
    def loop(sorted: List[Int], acc: List[List[Int]]): List[List[Int]] =
      sorted match
        case Nil => acc.reverse
        case head :: tail => loop(tail, group(acc, head))
    loop(nums.sorted, Nil)
      .map(g => if g.size == 1 then g.head.toString else s"${g.head}–${g.last}")
      .mkString(", ")