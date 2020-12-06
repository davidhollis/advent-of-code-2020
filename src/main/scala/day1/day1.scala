package day1

import scala.io.Source

class ExpenseReportValidator(
    expenses: Seq[Int]
) {
  def findSetSummingTo(sum: Int, n: Int): Option[Seq[Int]] = {
    if (n == 1) {
      expenses.find(_ == sum).map(n => Seq(n))
    } else {
      val (smolEntries, lorgEntries) =
        expenses.partition(_ < (sum / 2))

      lorgEntries.flatMap { l =>
        val innerValidator =
          new ExpenseReportValidator(expenses.filterNot(_ == l))
        innerValidator.findSetSummingTo(sum - l, n - 1).map(l +: _)
      }.headOption
    }
  }
}

object Main extends App {
  val expenseFilePath = "src/main/scala/day1/expenses.txt"
  val desiredSum = 2020

  val expenses =
    Source
      .fromFile(expenseFilePath)
      .getLines()
      .flatMap(_.toIntOption)
      .toSeq
  val idator = new ExpenseReportValidator(expenses)

  val Some(Seq(a1, b1)) = idator.findSetSummingTo(desiredSum, n = 2)
  println(s"$a1 * $b1 = ${a1 * b1}")

  val Some(Seq(a2, b2, c2)) = idator.findSetSummingTo(desiredSum, n = 3)
  println(s"$a2 * $b2 * $c2 = ${a2 * b2 * c2}")
}
