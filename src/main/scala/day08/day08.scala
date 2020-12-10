package day08

import scala.annotation.tailrec
import scala.io.Source

case class State(pc: Int, acc: Int)

object State {
  val initial: State = State(0, 0)
}

sealed trait Operation {
  def run(initialState: State): State
  def flip: Operation
}

trait OperationType {
  def apply(arg: Int): Operation
}

case class Acc(arg: Int) extends Operation {
  def run(initialState: State): State =
    State(initialState.pc + 1, initialState.acc + arg)

  def flip: Operation = this
}

object Acc extends OperationType

case class Jmp(arg: Int) extends Operation {
  def run(initialState: State): State =
    State(initialState.pc + arg, initialState.acc)

  def flip: Operation = Nop(arg)
}

object Jmp extends OperationType

case class Nop(arg: Int) extends Operation {
  def run(initialState: State): State =
    State(initialState.pc + 1, initialState.acc)

  def flip: Operation = Jmp(arg)
}

case object Nop extends OperationType

object OperationType {
  def unapply(opName: String): Option[OperationType] = opName match {
    case "acc" => Some(Acc)
    case "jmp" => Some(Jmp)
    case "nop" => Some(Nop)
    case _     => None
  }
}

object Operation {
  private val opRegex = """^(acc|jmp|nop) ((?:\+|-)[0-9]+)$""".r

  def unapply(line: String): Option[Operation] = line match {
    case opRegex(OperationType(tpe), argStr) =>
      argStr.toIntOption.map(tpe.apply)
    case _ => None
  }
}

sealed trait ExecutionResult

case class Halt(state: State) extends ExecutionResult

case class Crash(state: State) extends ExecutionResult

case class Loop(state: State) extends ExecutionResult

case class HandheldEmulator(
    program: Seq[Operation],
    state: State = State.initial,
    instructionsExecuted: Set[Int] = Set.empty
) {
  lazy val advance: HandheldEmulator = program.lift(state.pc) match {
    case Some(operation) =>
      copy(
        state = operation.run(state),
        instructionsExecuted = instructionsExecuted + state.pc
      )
    case None => this
  }

  lazy val halted: Boolean = state.pc == program.length
  lazy val crashed: Boolean = !program.indices.contains(state.pc)
  lazy val looping: Boolean = instructionsExecuted.contains(state.pc)
}

object HandheldEmulator {
  @tailrec
  def run(emulator: HandheldEmulator): ExecutionResult = {
    if (emulator.halted) {
      Halt(emulator.state)
    } else if (emulator.crashed) {
      Crash(emulator.state)
    } else if (emulator.looping) {
      Loop(emulator.state)
    } else {
      run(emulator.advance)
    }
  }

  def fixProgram(program: Seq[Operation]): Option[ExecutionResult] =
    program.zipWithIndex.foldLeft[Option[ExecutionResult]](None) {
      case (result, (op, idx)) =>
        result.orElse {
          val patchedProgram = program.patch(idx, Seq(op.flip), 1)
          run(HandheldEmulator(patchedProgram)) match {
            case success @ Halt(_) => Some(success)
            case _                 => None
          }
        }
    }
}

object Main extends App {
  val programPath = "src/main/scala/day08/program.txt"

  val program =
    Source
      .fromFile(programPath)
      .getLines()
      .flatMap(Operation.unapply)
      .toSeq
  println("Program:")
  program.foreach(op => println(s"  $op"))

  val machine = HandheldEmulator(program)
  val result = HandheldEmulator.run(machine)

  println()
  println(s"Result: $result")

  println()
  HandheldEmulator.fixProgram(program) match {
    case Some(result) => println(s"Fixed result: $result")
    case None         => println("No fix found")
  }
}
