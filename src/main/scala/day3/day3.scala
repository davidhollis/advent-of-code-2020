package day3

import scala.io.Source
import scala.annotation.tailrec

sealed abstract class MapEntity
case object Snow extends MapEntity
case object Tree extends MapEntity

object MapEntity {
  def parse(char: Char): MapEntity = char match {
    case '#' => Tree
    case _   => Snow
  }
}

case class Position(x: Int, y: Int)
case class Velocity(dx: Int, dy: Int)

class SlopeMap(
    rows: Int,
    columns: Int,
    map: Array[Array[MapEntity]]
) {
  def apply(pos: Position): Option[MapEntity] =
    map.lift(pos.y).flatMap(_.lift(pos.x % columns))

  def move(from: Position, by: Velocity): Position =
    Position((from.x + by.dx) % columns, from.y + by.dy)

  def hasEscaped(pos: Position): Boolean = pos.y >= rows
}

object SlopeMap {
  def fromSource(source: Source): SlopeMap = {
    val lines = source.getLines().toSeq
    new SlopeMap(
      lines.length,
      lines.headOption.map(_.length).getOrElse(1),
      lines
        .map(l => l.iterator.map(MapEntity.parse).toArray[MapEntity])
        .iterator
        .toArray[Array[MapEntity]]
    )
  }

  @tailrec
  def countTrees(
      map: SlopeMap,
      startingPoint: Position,
      velocity: Velocity,
      treesAlreadyHit: Int = 0
  ): Int = map(startingPoint) match {
    case None => treesAlreadyHit
    case Some(Tree) =>
      countTrees(
        map,
        map.move(from = startingPoint, by = velocity),
        velocity,
        treesAlreadyHit + 1
      )
    case Some(_) =>
      countTrees(
        map,
        map.move(from = startingPoint, by = velocity),
        velocity,
        treesAlreadyHit
      )
  }
}

object Main extends App {
  val mapFilePath = "src/main/scala/day3/map.txt"
  val velocities = Seq(
    Velocity(1, 1),
    Velocity(3, 1),
    Velocity(5, 1),
    Velocity(7, 1),
    Velocity(1, 2)
  )

  val map = SlopeMap.fromSource(Source.fromFile(mapFilePath))
  val possibilities = velocities.map { v =>
    v -> SlopeMap.countTrees(
      map,
      startingPoint = Position(0, 0),
      velocity = v
    )
  }.toMap
  println("Possibilities:")
  possibilities.foreach { case (v, trees) =>
    println(s"  right ${v.dx}, down ${v.dy}: hits $trees trees")
  }
  println()
  println(s"Product: ${possibilities.values.product}")
}
