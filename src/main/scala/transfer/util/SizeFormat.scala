package transfer.util

/** Разбор и форматирование размеров (байт, КБ, МБ, ГБ, ТБ). */
object SizeFormat:

  val KiB: Long = 1024L
  val MiB: Long = KiB * 1024L
  val GiB: Long = MiB * 1024L
  val TiB: Long = GiB * 1024L

  //получает строку и пытается ее превратить в колво байт
  def parseSize(s: String): Option[Long] =
    val t = s.trim
    if t.isEmpty then None
    else
      val (numPart, unitPart) = t.span(c => c.isDigit || c == '.' || c == ',')
      val number = numPart.trim.replace(',', '.').toDoubleOption.filter(_ >= 0)//пытается превратить строку в число с плавающей точкой Double.
      val multiplier = unitPart.trim.toUpperCase match//toUpperCase фигачит все в верхний регистр
        case "" | "B"               => Some(1L)
        case "KB" | "КБ"            => Some(KiB)
        case "MB" | "МБ"            => Some(MiB)
        case "GB" | "ГБ"            => Some(GiB)
        case "TB" | "ТБ"            => Some(TiB)
        case _                      => None
      for
        n <- number
        m <- multiplier
      yield (n * m).toLong

  //Форматирует размер в человекочитаемый вид, например 
  def humanReadable(bytes: Long): String =
    def valOf(unit: String, scale: Long) = f"${bytes.toDouble / scale}%.1f $unit".replace('.', ',')
    if bytes >= TiB then valOf("ТБ", TiB)
    else if bytes >= GiB then valOf("ГБ", GiB)
    else if bytes >= MiB then valOf("МБ", MiB)
    else if bytes >= KiB then valOf("КБ", KiB)
    else s"$bytes Б"