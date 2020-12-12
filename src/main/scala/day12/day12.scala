package day12

import scala.io.Source

object Instruction {
  val regex = """^([A-Z])([0-9]+)$""".r
  def unapply(line: String): Option[(String, Int)] = line match {
    case regex(action, argument) => argument.toIntOption.map(action -> _)
    case _                       => None
  }
}

trait Leftable[A] {
  def left: A
}

case class ShipVector(dx: Int, dy: Int) extends Leftable[ShipVector] {
  def +(other: ShipVector): ShipVector =
    ShipVector(dx + other.dx, dy + other.dy)
  def *(scale: Int): ShipVector = ShipVector(dx * scale, dy * scale)
  def left: ShipVector = ShipVector(-dy, dx)
}

object ShipVector {
  def unapply(line: String): Option[ShipVector] = line match {
    case Instruction(Direction(dir), magnitude) => Some(dir * magnitude)
    case _                                      => None
  }
}

sealed abstract class Direction(val actionSymbol: String, dx: Int, dy: Int)
    extends Leftable[Direction] {
  def *(magnitude: Int): ShipVector = ShipVector(dx * magnitude, dy * magnitude)
  def left: Direction
}

case object North extends Direction("N", 0, 1) {
  lazy val left: Direction = West
}
case object East extends Direction("E", 1, 0) {
  lazy val left: Direction = North
}
case object South extends Direction("S", 0, -1) {
  lazy val left: Direction = East
}
case object West extends Direction("W", -1, 0) {
  lazy val left: Direction = South
}

object Direction {
  lazy val all: Set[Direction] = Set(North, East, South, West)
  lazy val byActionSymbol: Map[String, Direction] =
    all.map(d => d.actionSymbol -> d).toMap
  def unapply(action: String): Option[Direction] = byActionSymbol.get(action)
}

sealed trait Turn {
  def apply[L <: Leftable[L]](l: L): L
}

case object Left0 extends Turn {
  def apply[L <: Leftable[L]](l: L): L = l
}

case object Left90 extends Turn {
  def apply[L <: Leftable[L]](l: L): L = l.left
}

case object Left180 extends Turn {
  def apply[L <: Leftable[L]](l: L): L = Left90(Left90(l))
}
case object Left270 extends Turn {
  def apply[L <: Leftable[L]](l: L): L = Left90(Left90(Left90(l)))
}

object Turn {
  def left(degrees: Int): Turn = ((degrees % 360) / 90) match {
    case 0 => Left0
    case 1 => Left90
    case 2 => Left180
    case 3 => Left270
  }

  def right(degrees: Int): Turn = left(360 - degrees)

  def unapply(line: String): Option[Turn] = line match {
    case Instruction("L", degrees) => Some(left(degrees))
    case Instruction("R", degrees) => Some(right(degrees))
    case _                         => None
  }
}

case class Ship(x: Int = 0, y: Int = 0, direction: Direction = East) {
  def move(vec: ShipVector): Ship = copy(x = x + vec.dx, y = y + vec.dy)
  def turn(by: Turn): Ship = copy(direction = by.apply(direction))
  def forward(by: Int): Ship = move(direction * by)

  def perform(instruction: String): Ship = instruction match {
    case ShipVector(vec)          => move(vec)
    case Turn(t)                  => turn(t)
    case Instruction("F", amount) => forward(amount)
    case _                        => this
  }

  def manhattanDistanceFromOrigin: Int = x.abs + y.abs
}

case class WaypointShip(
    x: Int = 0,
    y: Int = 0,
    waypoint: ShipVector = ShipVector(10, 1)
) {
  def moveWaypoint(vec: ShipVector): WaypointShip =
    copy(waypoint = waypoint + vec)
  def rotateWaypointAboutShip(by: Turn): WaypointShip =
    copy(waypoint = by.apply(waypoint))
  def forward(by: Int): WaypointShip = {
    val movement = waypoint * by
    copy(
      x = x + movement.dx,
      y = y + movement.dy
    )
  }

  def perform(instruction: String): WaypointShip = instruction match {
    case ShipVector(vec)          => moveWaypoint(vec)
    case Turn(t)                  => rotateWaypointAboutShip(t)
    case Instruction("F", amount) => forward(amount)
    case _                        => this
  }

  def manhattanDistanceFromOrigin: Int = x.abs + y.abs
}

object Main extends App {
  val instructionsPath = "src/main/scala/day12/instructions.txt"

  val instructions = Source
    .fromFile(instructionsPath)
    .getLines()
    .toSeq

  val initialState = Ship()
  val finalState = instructions.foldLeft(initialState)(_ perform _)

  println(s"Initial: $initialState")
  println(s"Final: $finalState")
  println(
    s"Manhattan Distance Traveled: ${finalState.manhattanDistanceFromOrigin}"
  )

  val initialStateWithWaypoint = WaypointShip()
  val finalStateWithWaypoint =
    instructions.foldLeft(initialStateWithWaypoint)(_ perform _)
  println()
  println(s"Initial with waypoint: $initialStateWithWaypoint")
  println(s"Final with waypoint: $finalStateWithWaypoint")
  println(
    s"Manhattan Distance Traveled: ${finalStateWithWaypoint.manhattanDistanceFromOrigin}"
  )
}
