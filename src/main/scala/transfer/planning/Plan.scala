package transfer.planning

import transfer.domain.{Media, Part}

//файл описывает структуры данных для плана распределения файлов по носителям. Он хранит информацию о том, какие части данных будут записаны на каждый носитель и весь план целикомфайл описывает структуры данных для плана распределения файлов по носителям. Он хранит информацию о том, 
//какие части данных будут записаны на каждый носитель и весь план целиком

//Хранит результат распределения данных для одного носителя
case class MediaAllocation(
    mediaNumber: Int,
    partNumbers: List[Int],
    writtenBytes: Long,//сколько байт будет записано
    remainingBytes: Long//сколько места останется
)

//Хранит полный план записи набора данных
case class Plan(
    setId: String,
    name: String,
    totalSize: Long,
    partSize: Long,
    parts: List[Part],
    allocations: List[MediaAllocation],//распределение частей по носителям
    warnings: List[String],
    mediaLabels: List[String] = Nil//названия носителей в порядке их заполнения
)