package day05

import scala.util.Try
import scala.io.Source

case class Seat(row: Int, column: Int) {
  def id: Int = row * 8 + column
  def valid: Boolean = (0 to 127).contains(row) && (0 to 7).contains(column)
}

object Seat {
  def fromString(seatString: String): Option[Seat] = {
    val rowBinary =
      seatString.substring(0, 7).replace('F', '0').replace('B', '1')
    val colBinary =
      seatString.substring(7, 10).replace('L', '0').replace('R', '1')

    for {
      row <- Try(Integer.parseInt(rowBinary, 2)).toOption
      col <- Try(Integer.parseInt(colBinary, 2)).toOption
    } yield Seat(row, col)
  }
}

object Main extends App {
  val seatFilePath = "src/main/scala/day05/seats.txt"

  val seats =
    Source
      .fromFile(seatFilePath)
      .getLines()
      .flatMap(Seat.fromString)
      .toSeq
      .sortBy(_.id)

  seats.foreach { seat => println(s"$seat id=${seat.id}") }
  println()
  println(s"Max id = ${seats.last.id}")
  println()
  println()

  for (i <- 1 until seats.length) {
    val (prev, cur) = (seats(i - 1), seats(i))
    if (cur.id - prev.id > 1) {
      println(
        s"Missing seat with id ${cur.id - 1} between [$prev id=${prev.id}] and [$cur id=${cur.id}]"
      )
    }
  }

}
