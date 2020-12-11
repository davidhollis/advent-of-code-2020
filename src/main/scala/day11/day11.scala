package day11

import scala.io.Source
import scala.annotation.tailrec

sealed abstract class CellStatus(val symbol: Char)

case object Floor extends CellStatus('.')
case object Vacant extends CellStatus('L')
case object Occupied extends CellStatus('#')

object CellStatus {
  val all: Set[CellStatus] = Set(Floor, Vacant, Occupied)
  lazy val bySymbol: Map[Char, CellStatus] = all.map(s => s.symbol -> s).toMap
}

case class Seating(grid: Seq[Seq[CellStatus]], nrows: Int, ncols: Int) {
  def prettyPrint: String = grid.map(_.map(_.symbol).mkString).mkString("\n")

  def count(pred: CellStatus => Boolean): Int = grid.foldLeft(0) { (acc, row) =>
    acc + row.count(pred)
  }

  @inline
  def cellAt(row: Int, col: Int): Option[CellStatus] =
    grid.lift(row).flatMap(_.lift(col))

  def advance(computeCell: (Int, Int) => CellStatus): Seating = {
    val newGrid = (0 until nrows).map { row =>
      (0 until ncols).map[CellStatus] { col =>
        computeCell(row, col)
      }
    }
    Seating(newGrid, nrows, ncols)
  }

  def checkImmediateSurroundings(row: Int, col: Int): CellStatus =
    cellAt(row, col) match {
      case Some(Vacant) =>
        if (surroundingCells(row, col).count(_ == Occupied) == 0) Occupied
        else Vacant
      case Some(Occupied) =>
        if (surroundingCells(row, col).count(_ == Occupied) >= 4) Vacant
        else Occupied
      case _ => Floor
    }

  def checkNearestSeats(row: Int, col: Int): CellStatus =
    cellAt(row, col) match {
      case Some(Vacant) =>
        if (surroundingSeats(row, col).count(_ == Occupied) == 0) Occupied
        else Vacant
      case Some(Occupied) =>
        if (surroundingSeats(row, col).count(_ == Occupied) >= 5) Vacant
        else Occupied
      case _ => Floor
    }

  private def surroundingCells(row: Int, col: Int): Seq[CellStatus] =
    for {
      (rowOffset, colOffset) <- Seating.adjacentOffsets
      cell <- cellAt(row + rowOffset, col + colOffset)
    } yield cell

  private def surroundingSeats(row: Int, col: Int): Seq[CellStatus] =
    for {
      direction <- Seating.adjacentOffsets
      cell <- nextVisibleSeat(from = (row, col), direction = direction)
    } yield cell

  @tailrec
  private def nextVisibleSeat(
      from: (Int, Int),
      direction: (Int, Int)
  ): Option[CellStatus] = {
    val nextRow = from._1 + direction._1
    val nextCol = from._2 + direction._2
    val nextCell = cellAt(nextRow, nextCol)
    nextCell match {
      case Some(Floor) =>
        nextVisibleSeat(
          (nextRow, nextCol),
          direction
        )
      case cell => cell
    }
  }
}

object Seating {
  val adjacentOffsets: Seq[(Int, Int)] = {
    val offsets = Seq(-1, 0, 1)
    for {
      x <- offsets
      y <- offsets
      if (x != 0) || (y != 0)
    } yield (x, y)
  }

  def parseLine(line: String): Seq[CellStatus] =
    line.flatMap(CellStatus.bySymbol.get)

  def fromSource(source: Source): Seating = {
    val grid = source.getLines().map(parseLine).toSeq
    Seating(
      grid,
      grid.size,
      grid.map(_.size).max
    )
  }
}

class SeatingSimulator(initialSeating: Seating, scenario: Int)
    extends Iterator[Seating] {
  var upNext: Option[Seating] = Some(initialSeating)

  def hasNext: Boolean = upNext.isDefined

  def next(): Seating = {
    val result = upNext.get
    val advanced = computeNext(result)
    if (result == advanced) {
      upNext = None
    } else {
      upNext = Some(advanced)
    }

    result
  }

  private def computeNext(current: Seating): Seating = scenario match {
    case 1 => current.advance(current.checkImmediateSurroundings)
    case 2 => current.advance(current.checkNearestSeats)
    case _ => current
  }
}

object Main extends App {
  val initialSeatingPath = "src/main/scala/day11/initial-seating.txt"

  val initialSeating = Seating.fromSource(Source.fromFile(initialSeatingPath))
  val simulator = new SeatingSimulator(initialSeating, scenario = 2)
  simulator.zipWithIndex.foreach { case (state, step) =>
    println(s"=== Step $step ===")
    println(s">> ${state.count(_ == Occupied)} occupied seats")
    println(state.prettyPrint)
    println()
  }
}
