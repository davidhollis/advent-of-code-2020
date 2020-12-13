package day13

import scala.io.Source

sealed trait Bus {
  def inService: Boolean
  def earliestDepartureAfter(time: Int): Int
  def id: Int
}

case class ScheduledBus(id: Int) extends Bus {
  def inService: Boolean = true
  def earliestDepartureAfter(time: Int): Int = id * ((time / id) + 1)
}

case object NoBus extends Bus {
  def inService: Boolean = false
  def earliestDepartureAfter(time: Int): Int = 0
  def id: Int = -1
}

object Bus {
  def apply(specification: String): Bus =
    specification.toIntOption.fold[Bus](NoBus)(ScheduledBus)
}

case class EquivalenceRelation(value: Long, modulus: Long) {
  override def toString: String = s"t === $value (mod $modulus)"

  def &&(other: EquivalenceRelation): EquivalenceRelation = {
    val soln = EquivalenceRelation(
      value = (value to (modulus * other.modulus) by modulus)
        .find(a => (a % other.modulus) == (other.value % other.modulus))
        .get,
      modulus = (modulus * other.modulus)
    )
    println(s"[$this] && [$other] => [$soln]")
    soln
  }
}

object Main extends App {
  val notesPath = "src/main/scala/day13/notes.txt"

  val noteSource = Source.fromFile(notesPath).getLines()

  val arrival = noteSource.next().toInt
  val busses = noteSource.next().split(",").map(Bus.apply)

  val nextBus =
    busses.filter(_.inService).minBy(_.earliestDepartureAfter(arrival))
  val departure = nextBus.earliestDepartureAfter(arrival)

  println(
    s"Best bus is the ${nextBus.id} departing at t=$departure, ${departure - arrival} minutes after arrival."
  )
  println(
    s">> ${departure - arrival} * ${nextBus.id} = ${(departure - arrival) * nextBus.id}"
  )

  println()
  println("=== Finding Schedule Pattern ===")
  val system = busses.zipWithIndex
    .flatMap {
      case (ScheduledBus(id), idx) =>
        Some(EquivalenceRelation((idx * id - idx) % id, id))
      case _ => None
    }
    .sortBy(-_.modulus)
  println(">> Solving the following system:")
  system.foreach(rel => println(s">>  $rel"))

  val solved = system.reduceLeft[EquivalenceRelation](_ && _)
  println(s">>> Solved: $solved")
}
