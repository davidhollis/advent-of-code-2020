package day02

import scala.io.Source

case class PasswordPolicy(character: Char, frequency: Range) {
  def acceptsOld(password: String): Boolean =
    frequency.contains(password.count(_ == character))

  def acceptsCurrent(password: String): Boolean =
    (
      password(frequency.start - 1) == character
    ) ^ (
      password(frequency.end - 1) == character
    )
}

object PasswordAndPolicy {
  val LineRegex = """^([0-9]+)-([0-9]+) (.): (.*)$""".r

  def unapply(line: String): Option[(String, PasswordPolicy)] = line match {
    case LineRegex(lowerBound, upperBound, character, password) =>
      for {
        lb <- lowerBound.toIntOption
        ub <- upperBound.toIntOption
      } yield (password, PasswordPolicy(character(0), lb to ub))
    case _ => None
  }
}

object Main extends App {
  val passwordFilePath = "src/main/scala/day2/passwords.txt"

  val valid = Source
    .fromFile(passwordFilePath)
    .getLines()
    .flatMap(PasswordAndPolicy.unapply)
    .filter { case (password, policy) => policy.acceptsCurrent(password) }
    .toSeq

  valid.foreach { case (password, policy) => println(s"$policy: $password") }
  println(s"==> ${valid.length} valid passwords")
}
