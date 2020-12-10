package day09

import day01.ExpenseReportValidator
import scala.io.Source

case class XmasStream(values: Seq[Int], windowSize: Int) {
  def firstInvalidEntry: Option[Int] =
    values.indices.iterator
      .drop(windowSize)
      .find { idx =>
        val window = values.drop(idx - windowSize).take(windowSize)
        val sumChecker = new ExpenseReportValidator(window)
        val pair = sumChecker.findSetSummingTo(values(idx), n = 2)

        pair.isEmpty
      }

  def matchingSequence(targetSum: Int): Option[Seq[Int]] =
    indexTriangle
      .map { case (start, end) =>
        values.drop(start).take(end - start)
      }
      .find(_.sum == targetSum)

  private def indexTriangle: Iterable[(Int, Int)] =
    for {
      i <- 0 until values.length
      j <- (i + 1) until values.length
    } yield (i, j)
}

object Main extends App {
  val streamPath = "src/main/scala/day9/xmas-stream.txt"
  val windowSize = 25

  val stream = XmasStream(
    Source.fromFile(streamPath).getLines().flatMap(_.toIntOption).toSeq,
    windowSize
  )
  stream.firstInvalidEntry.foreach { firstInvalid =>
    println(
      s"First invalid entry at index $firstInvalid: ${stream.values(firstInvalid)}"
    )

    stream.matchingSequence(stream.values(firstInvalid)).foreach {
      matchingSequence =>
        println(s"==> Found matching sequence $matchingSequence")
        println(s"==> with key ${matchingSequence.min + matchingSequence.max}")
    }
  }
}
