package day6

import scala.io.Source

case class PassengerGroup(affirmatives: Set[Char]) {
  def |+|(other: PassengerGroup): PassengerGroup = PassengerGroup(
    affirmatives ++ other.affirmatives
  )

  def &(other: PassengerGroup): PassengerGroup = PassengerGroup(
    affirmatives & other.affirmatives
  )
}

object PassengerGroup {
  def fromLine(line: String): Option[PassengerGroup] =
    if (line.isBlank()) None else Some(PassengerGroup(line.toSet[Char]))

  val empty: PassengerGroup = PassengerGroup(Set.empty)
  val full: PassengerGroup = PassengerGroup(('a' to 'z').toSet)

  case class ParserState(
      completed: Seq[PassengerGroup],
      current: PassengerGroup,
      disjunctive: Boolean
  ) {
    def +(passenger: PassengerGroup): ParserState =
      copy(current =
        if (disjunctive) current |+| passenger else current & passenger
      )
    def commit: ParserState =
      copy(
        completed = completed :+ current,
        current = if (disjunctive) PassengerGroup.empty else PassengerGroup.full
      )
  }

  object ParserState {
    def initial(disjunctive: Boolean): ParserState =
      ParserState(
        Seq.empty,
        if (disjunctive) PassengerGroup.empty else PassengerGroup.full,
        disjunctive
      )
  }

  def fromSource(
      source: Source,
      disjunctive: Boolean = true
  ): Seq[PassengerGroup] =
    source
      .getLines()
      .foldLeft(ParserState.initial(disjunctive)) { case (state, line) =>
        PassengerGroup.fromLine(line) match {
          case Some(group) => state + group
          case None        => state.commit
        }
      }
      .commit
      .completed
}

object CustomsForm extends App {
  val passengerSurveyFilePath = "src/main/scala/day6/survey.txt"

  val groups =
    PassengerGroup.fromSource(
      Source.fromFile(passengerSurveyFilePath),
      disjunctive = false
    )
  println(s"Sum of affirmative counts: ${groups.map(_.affirmatives.size).sum}")
}
