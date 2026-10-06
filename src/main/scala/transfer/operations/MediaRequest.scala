package transfer.operations

import transfer.domain.{AppError, Media}

//файл описывает механизм получения пути к нужному носителю. 
//Он позволяет программе либо запросить у пользователя путь к очередному носителю, 
//либо автоматически использовать уже известный путь

// Определяет способ получения фактического пути к носителю во время работы программы.
trait MediaRequest:

  /**
   * Возвращает фактический путь к носителю с номером {@code number}.
   *
   * @param number   порядковый номер носителя, начиная с 1
   * @param total    сколько носителей в наборе
   * @param expected носитель, запланированный пользователем
  */
  def path(number: Int, total: Int, expected: Media): Either[AppError, String]

   // Использует заранее известный путь к носителю без запроса пользователя
  object NoRequest extends MediaRequest:
    def path(number: Int, total: Int, expected: Media): Either[AppError, String] = Right(expected.path)
