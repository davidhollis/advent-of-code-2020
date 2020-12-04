package day4

import scala.io.Source

case class PassportData(fields: Map[String, String]) {
  def |+|(other: PassportData): PassportData =
    copy(fields = fields ++ other.fields)

  lazy val isEmpty: Boolean = fields.isEmpty

  lazy val validate: PassportData.ValidationResult =
    PassportData.requiredFields.foldLeft[PassportData.ValidationResult](
      PassportData.Valid
    ) { case (evaluation, (requiredFieldName, validationRule)) =>
      fields.get(requiredFieldName) match {
        case Some(value) =>
          if (validationRule(value)) evaluation
          else evaluation.withError(s"invalid ${requiredFieldName}")
        case None => evaluation.withError(s"missing ${requiredFieldName}")
      }
    }
}

object PassportData {
  private val heightRegex = """^([0-9]+)(in|cm)$""".r
  private val colorRegex = """^#[0-9a-f]{6}$""".r
  private val eyeColors = Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  private val passportIdRegex = """^[0-9]{9}$""".r
  val requiredFields: Map[String, String => Boolean] = Map(
    "byr" -> (byr =>
      (byr.length == 4) &&
        byr.toIntOption
          .map(yr => (1920 <= yr) && (yr <= 2002))
          .getOrElse(false)
    ),
    "iyr" -> (iyr =>
      (iyr.length == 4) &&
        iyr.toIntOption
          .map(yr => (2010 <= yr) && (yr <= 2020))
          .getOrElse(false)
    ),
    "eyr" -> (eyr =>
      (eyr.length == 4) &&
        eyr.toIntOption
          .map(yr => (2020 <= yr) && (yr <= 2030))
          .getOrElse(false)
    ),
    "hgt" -> {
      case heightRegex(number, "in") =>
        number.toIntOption
          .map(in => (59 <= in) && (in <= 76))
          .getOrElse(false)
      case heightRegex(number, "cm") =>
        number.toIntOption
          .map(cm => (150 <= cm) && (cm <= 193))
          .getOrElse(false)
      case _ => false
    },
    "hcl" -> (colorRegex.matches),
    "ecl" -> (eyeColors.contains),
    "pid" -> (passportIdRegex.matches)
  )

  sealed trait ValidationResult {
    def withError(err: String): ValidationResult
    val label: String
    val isValid: Boolean
    val errors: Seq[String]
  }
  case object Valid extends ValidationResult {
    def withError(err: String): ValidationResult = NotValid(Seq(err))
    val label: String = "[   VALID   ]"
    val isValid: Boolean = true
    val errors: Seq[String] = Seq.empty
  }
  case class NotValid(errors: Seq[String]) extends ValidationResult {
    def withError(err: String): ValidationResult = copy(errors = errors :+ err)
    val label: String = "[ NOT VALID ]"
    val isValid: Boolean = false
  }

  def parseLine(line: String): PassportData = PassportData(
    line
      .split("""\s+""")
      .flatMap { part =>
        part.split(":", 2) match {
          case Array(fieldName, fieldValue) => Some(fieldName -> fieldValue)
          case _                            => None
        }
      }
      .toMap[String, String]
  )

  val empty: PassportData = PassportData(Map.empty)

  case class ParserState(built: Seq[PassportData], current: PassportData) {
    def +(partial: PassportData): ParserState =
      copy(current = current |+| partial)
    def commit: ParserState =
      copy(
        built = if (current.isEmpty) built else built :+ current,
        current = PassportData.empty
      )
  }
  object ParserState {
    val empty: ParserState = ParserState(Seq.empty, PassportData.empty)
  }
  def fromSource(source: Source): Seq[PassportData] =
    source
      .getLines()
      .foldLeft[ParserState](ParserState.empty) { case (state, line) =>
        if (line.isBlank())
          state.commit
        else
          state + PassportData.parseLine(line)
      }
      .commit
      .built
}

object PassportValidator extends App {
  val passportDataFeedFile: String = "src/main/scala/day4/passports.txt"
  val validityLabels: Map[Boolean, String] = Map(
    true -> "[   VALID   ]",
    false -> "[ NOT VALID ]"
  )

  val passports = PassportData.fromSource(Source.fromFile(passportDataFeedFile))

  println("Passports:")
  passports.foreach { p =>
    println(s"  ${p.validate.label} ${p}")
    p.validate.errors.foreach { err =>
      println(s"    <!> $err")
    }
  }
  println()
  println(s"Total valid passports: ${passports.count(_.validate.isValid)}")
}
