package day10

import scala.io.Source

case class AdapterSequence(adapters: Seq[Int]) {
  import AdapterSequence._

  def differences: Seq[Int] =
    adapters.foldLeft[Diffs](Diffs())(_ append _).differences

  def countArrangements: Long = {
    val device = adapters.last
    adapters
      .dropRight(1)
      .foldRight(Map[Int, Long](device -> 1L)) { (dongle, memo) =>
        val thisDongleCount =
          validIncrements.map(incr => memo.getOrElse(dongle + incr, 0L)).sum
        memo + (dongle -> thisDongleCount)
      }
      .apply(0)
  }
}

object AdapterSequence {
  val validIncrements: Seq[Int] = Seq(1, 2, 3)

  private case class Diffs(
      differences: Seq[Int] = Seq.empty,
      lastValue: Option[Int] = None
  ) {
    def append(nextValue: Int): Diffs = lastValue match {
      case Some(last) =>
        copy(
          differences = differences :+ (nextValue - last),
          lastValue = Some(nextValue)
        )
      case None => copy(lastValue = Some(nextValue))
    }
  }

  def fromSource(source: Source): AdapterSequence = {
    val baseSeq = source.getLines().flatMap(_.toIntOption).toSeq.sorted
    AdapterSequence(0 +: baseSeq :+ (baseSeq.last + 3))
  }
}

object Main extends App {
  val adapterListPath = "src/main/scala/day10/adapters.txt"

  val adapters = AdapterSequence.fromSource(Source.fromFile(adapterListPath))
  val differences = adapters.differences
  val ones = differences.count(_ == 1)
  val threes = differences.count(_ == 3)

  println(s"Joltage differences: $ones of 1 and $threes of 3")
  println(s"[$ones * $threes = ${ones * threes}]")

  println()
  println(
    s"There are ${adapters.countArrangements} distinct adapter arrangements."
  )
}
