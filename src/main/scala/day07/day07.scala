package day07

import util.{Semigroup, LineParser}
import scala.io.Source
import scala.annotation.tailrec

case class BagColor(description: String)

case class BagRule(contents: Map[BagColor, Int])

object BagRule {
  val bagSpecRegex = """^([0-9]+) (.*) bags?$""".r

  def unapply(ruleStr: String): Option[BagRule] = ruleStr match {
    case "no other bags" => Some(BagRule(Map.empty))
    case _ =>
      Some(
        BagRule(
          ruleStr
            .split(", ")
            .flatMap {
              case bagSpecRegex(numberStr, colorStr) =>
                numberStr.toIntOption.map(count =>
                  (BagColor(colorStr) -> count)
                )
              case _ => None
            }
            .toMap
        )
      )
  }

  val empty: BagRule = BagRule(Map.empty)
}

case class RuleSet(rules: Map[BagColor, BagRule]) {
  def union(other: RuleSet): RuleSet = copy(rules = rules ++ other.rules)

  def reverseContainmentRelation: BagContainmentRelation = {
    val containment = for {
      (outerBagColor, BagRule(contents)) <- rules.toSeq
      (innerBagColor, _) <- contents
    } yield innerBagColor -> outerBagColor

    BagContainmentRelation(
      containment.groupBy(_._1).map { case (innerColor, colorPairs) =>
        innerColor -> colorPairs.map(_._2).toSet
      }
    )
  }

  def recursiveBagCount(color: BagColor): Int =
    rules.getOrElse(color, BagRule.empty).contents.foldLeft(0) {
      case (sumSoFar, (innerColor, count)) =>
        sumSoFar + count + count * recursiveBagCount(innerColor)
    }
}

case class BagContainmentRelation(relation: Map[BagColor, Set[BagColor]]) {
  lazy val nextLevel: BagContainmentRelation = BagContainmentRelation(
    relation.map { case (innerColor, outerColors) =>
      innerColor -> outerColors.foldLeft[Set[BagColor]](outerColors) {
        (acc, outerColor) =>
          acc union relation.getOrElse(outerColor, Set.empty)
      }
    }
  )

  @tailrec
  final def transitiveClosure: BagContainmentRelation =
    if (this.isSameAs(nextLevel))
      this
    else
      nextLevel.transitiveClosure

  def isSameAs(other: BagContainmentRelation): Boolean =
    (
      relation.keys.toSet union other.relation.keys.toSet
    ).forall(color =>
      relation.getOrElse(color, Set.empty) == other.relation
        .getOrElse(color, Set.empty)
    )
}

object RuleSet {
  val ruleLineRegex = """^(.*) bags contain (.*)\.$""".r
  implicit val parser: LineParser[RuleSet] = LineParser[RuleSet] {
    line: String =>
      line match {
        case ruleLineRegex(colorName, BagRule(contents)) =>
          LineParser.Combine(RuleSet(BagColor(colorName) -> contents))
        case _ => LineParser.Noop
      }
  }

  implicit val rulesetSemigroup: Semigroup[RuleSet] = Semigroup[RuleSet](
    RuleSet(Map.empty[BagColor, BagRule]),
    (a: RuleSet, b: RuleSet) => a union b
  )

  def apply(singleRule: (BagColor, BagRule)): RuleSet = RuleSet(Map(singleRule))
}

object Main extends App {
  val ruleFilePath = "src/main/scala/day07/rules.txt"
  val ourBag = BagColor("shiny gold")

  val rules = LineParser.fromSource[RuleSet](Source.fromFile(ruleFilePath)).head
  for ((BagColor(name), BagRule(contents)) <- rules.rules) {
    println(s"$name -> $contents")
  }

  println()
  val initialContainment = rules.reverseContainmentRelation
  val finalContainment = initialContainment.transitiveClosure
  val potentialContainers = finalContainment.relation(ourBag)
  println(
    s"Our bag can go in ${potentialContainers.size} bags: $potentialContainers"
  )

  println()
  println(s"Our bag contains ${rules.recursiveBagCount(ourBag)} other bags")
}
