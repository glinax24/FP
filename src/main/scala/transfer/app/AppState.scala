package transfer.app

import transfer.config.Config
import transfer.domain.*
import transfer.operations.VerifyReport
import transfer.planning.Plan

//это текущее состояние программы. В нём хранится вся информация,
// с которой приложение работает в данный момент
case class AppState(
    config: Config,
    configPath: String,
    set: Option[TransferSet],
    selected: List[SelectedItem],
    media: List[Media],
    plan: Option[Plan],
    manifest: Option[Manifest],
    //План построен и подтверждён: запись без подтверждения запрещена
    planConfirmed: Boolean = false,
    // В наборе есть нечитаемые файлы, и пользователь ещё не принял решение 
    pendingUnreadable: Boolean = false,
    // В наборе есть конфликты относительных путей, и решение ещё не принято
    pendingConflicts: Boolean = false,
    // Результат последней проверки готового набора — попадает в итоговый отчёт 
    verifyReport: Option[VerifyReport] = None,
    // Ошибки последних операций: попадают в итоговый отчёт
    errors: List[String] = Nil,
    // Путь к манифесту набора — попадает в итоговый отчёт
    manifestPath: Option[String] = None)
