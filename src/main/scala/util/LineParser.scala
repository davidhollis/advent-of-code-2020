package util

import scala.io.Source

trait LineParser[T] {
  def fromLine(line: String): LineParser.Operation[T] =
    if (line.isBlank()) LineParser.Commit
    else LineParser.Combine(fromNonblankLine(line))
  def fromNonblankLine(line: String): T = ???
}

object LineParser {
  sealed trait Operation[+T]
  case class CombineAndCommit[T](value: T) extends Operation[T]
  case class Combine[T](value: T) extends Operation[T]
  case object Commit extends Operation[Nothing]
  case object Noop extends Operation[Nothing]

  private case class ParserState[T: Semigroup](completed: Seq[T], current: T) {
    def add(element: T): ParserState[T] =
      copy(current = Semigroup[T].combine(current, element))
    def commit: ParserState[T] =
      copy(
        completed = completed :+ current,
        current = Semigroup[T].zero
      )
  }

  private object ParserState {
    def initial[T: Semigroup]: ParserState[T] =
      ParserState(Seq.empty, Semigroup[T].zero)
  }

  def apply[T](implicit parser: LineParser[T]): LineParser[T] = parser
  def apply[T](parse: String => Operation[T]): LineParser[T] =
    new LineParser[T] {
      override def fromLine(line: String): Operation[T] = parse(line)
    }
  def withBlankLineTerminators[T](parse: String => T): LineParser[T] =
    new LineParser[T] {
      override def fromNonblankLine(line: String): T = parse(line)
    }

  def fromSource[T: Semigroup: LineParser](source: Source): Seq[T] =
    source
      .getLines()
      .foldLeft[ParserState[T]](ParserState.initial[T]) { (state, line) =>
        LineParser[T].fromLine(line) match {
          case CombineAndCommit(element) => state.add(element).commit
          case Combine(element)          => state.add(element)
          case Commit                    => state.commit
          case Noop                      => state
        }
      }
      .commit
      .completed
}
