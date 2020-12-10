package day06

import scala.io.Source

import util.{Semigroup, LineParser}

case class PassengerGroup(affirmatives: Set[Char]) {
  def union(other: PassengerGroup): PassengerGroup = PassengerGroup(
    affirmatives ++ other.affirmatives
  )

  def intersect(other: PassengerGroup): PassengerGroup = PassengerGroup(
    affirmatives & other.affirmatives
  )
}

object PassengerGroup {
  implicit val parser: LineParser[PassengerGroup] =
    LineParser.withBlankLineTerminators { line =>
      PassengerGroup(line.toSet[Char])
    }

  val unionSemigroup: Semigroup[PassengerGroup] =
    Semigroup[PassengerGroup](
      PassengerGroup(Set.empty),
      (a: PassengerGroup, b: PassengerGroup) => a union b
    )
  val intersectionSemigroup: Semigroup[PassengerGroup] =
    Semigroup[PassengerGroup](
      PassengerGroup(('a' to 'z').toSet),
      (a: PassengerGroup, b: PassengerGroup) => a intersect b
    )
}

object Main extends App {
  val passengerSurveyFilePath = "src/main/scala/day06/survey.txt"

  val groupsUnion =
    LineParser.fromSource[PassengerGroup](
      Source.fromFile(passengerSurveyFilePath)
    )(PassengerGroup.unionSemigroup, PassengerGroup.parser)
  println(
    s"Sum of affirmative counts (union): ${groupsUnion.map(_.affirmatives.size).sum}"
  )

  val groupsIntersection =
    LineParser.fromSource[PassengerGroup](
      Source.fromFile(passengerSurveyFilePath)
    )(PassengerGroup.intersectionSemigroup, PassengerGroup.parser)
  println(
    s"Sum of affirmative counts (intersection): ${groupsIntersection.map(_.affirmatives.size).sum}"
  )
}
