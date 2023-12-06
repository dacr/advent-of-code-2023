package day05

import zio.*
import zio.test.*
import zio.test.TestAspect.*
import scala.util.chaining.*

// ------------------------------------------------------------------------------
type Num = Long

case class Range(from: Num, length: Num) {
  val to = from + length - 1

  def contains(value: Num): Boolean = value >= from && value <= to

  def distance(value: Num): Num = if (contains(value)) value - from else 0

  def disjoined(other: Range): Boolean = from > other.to || to < other.from

  def intersect(other: Range): Option[Range] = {
    val left  = math.max(from, other.from)
    val right = math.min(to, other.to)
    if (left > right) None
    else Range.fromTo(left, right)
  }

  def exclude(other: Range): List[Range] = {
    if (disjoined(other)) Nil
    else {
      val leftFrom  = from
      val leftTo    = other.from - 1
      val rightFrom = other.to + 1
      val rightTo   = to
      val ranges    = Range.fromTo(leftFrom, leftTo) :: Range.fromTo(rightFrom, rightTo) :: Nil
      ranges.flatten
    }
  }

  override def toString = s"[$from,$to]"
}

object Range {
  def fromTo(from: Num, to: Num): Option[Range] = {
    if (from > to) None
    else Some(Range(from, 1L + to - from))
  }

  def fromList(input: List[Num]): Range = {
    input match {
      case from :: length :: Nil => Range(from, length)
    }
  }
}

case class Rule(destination: Range, source: Range) {
  def contains(value: Num): Boolean = source.contains(value)
  def convert(value: Num): Num      = if (contains(value)) destination.from + source.distance(value) else value
}

object Rule {
  def from(input: Array[Num]): Rule = {
    input match {
      case Array(destinationFrom, sourceFrom, length) =>
        Rule(Range(destinationFrom, length), Range(sourceFrom, length))
    }
  }
}

case class Conversion(inputType: String, outputType: String, rules: List[Rule]) {
  def convert(value: Num): Num = {
    rules.find(_.contains(value)) match {
      case Some(rule) => rule.convert(value)
      case None       => value
    }
  }

  def convertWorker(remainingRules: List[Rule], inputRanges: List[Range]): List[Range] = {
    remainingRules match {
      case Nil                => Nil
      case rule :: otherRules =>
        inputRanges match {
          case Nil                                             => Nil
          case range :: others if range.disjoined(rule.source) => convertWorker(otherRules, inputRanges)
          case range :: others                                 =>
            val commonRange          = range.intersect(rule.source)
            val outsideRange         = range.exclude(rule.source)
            val convertedCommonRange = commonRange.flatMap(cr => Range.fromTo(from = rule.convert(cr.from), to = rule.convert(cr.to)))
            convertedCommonRange ++: convertWorker(rules, outsideRange) ++: convertWorker(rules,others)
        }
    }
  }

  def convert(ranges: List[Range]): List[Range] = {
    convertWorker(rules, ranges)
  }
}

case class Almanac(seeds: List[Num], conversions: List[Conversion])

def parseConversion(spec: String): Conversion = {
  spec.split(" map:\n") match {
    case Array(header, rulesSpecs) =>
      val Array(inputType, outputType) = header.split("-to-", 2)

      val rules =
        rulesSpecs
          .split("\n")
          .toList
          .map(_.trim.split(" ").map(_.toLong))
          .map(Rule.from)
      Conversion(inputType, outputType, rules)
  }
}

def parse(input: String): Almanac = {
  val parts = input.split("\n\n").toList
  parts match {
    case header :: conversionsSpecs =>
      val seeds =
        header
          .replace("seeds: ", "")
          .trim
          .split(" ")
          .toList
          .map(_.toLong)

      val conversions =
        conversionsSpecs
          .map(parseConversion)

      Almanac(seeds, conversions)
  }
}

// ------------------------------------------------------------------------------

def convert(almanac: Almanac, value: Num, inputType: String, targetType: String): Num = {
  almanac.conversions.find(_.inputType == inputType) match {
    case Some(conversion) =>
      val newValue     = conversion.convert(value)
      val newInputType = conversion.outputType
      if (newInputType == targetType) newValue
      else convert(almanac, newValue, newInputType, targetType)
  }
}

def resolveStar1(input: String): Long =
  val almanac = parse(input)
  almanac.seeds
    .map(seed => convert(almanac, seed, "seed", "location"))
    .min

// ------------------------------------------------------------------------------

def convert(almanac: Almanac, ranges: List[Range], inputType: String, targetType: String): List[Range] = {
  almanac.conversions.find(_.inputType == inputType) match {
    case Some(conversion) =>
      val newRanges    = conversion.convert(ranges)
      val newInputType = conversion.outputType
      println(s"---------------------------------")
      println(s"$inputType=>$newInputType")
      println(s"   ${ranges.toString}")
      println(s"   ${newRanges.toString}")
      if (newInputType == targetType) newRanges
      else convert(almanac, newRanges, newInputType, targetType)
  }
}

def resolveStar2(input: String): Long = {
  val almanac = parse(input)
  almanac.seeds
    .sliding(2, 2)
    .toList
    .map(Range.fromList)
    .tap(r => println(s"RANGE $r"))
    .map(seedRange => convert(almanac, seedRange :: Nil, "seed", "location"))
    .flatMap(_.map(_.from))
    .min
}

// ------------------------------------------------------------------------------

object Puzzle05Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("algorithms") {
      val range = (a: Num, b: Num) => Range.fromTo(a, b).get
      assertTrue(
        range(0, 10).disjoined(range(20, 30)),
        !range(0, 10).disjoined(range(10, 20)),
        Range(100, 20) == range(100, 119),
        range(10, 20).distance(12) == 2,
        range(10, 20).distance(10) == 0,
        range(10, 20).exclude(range(20, 20)) == List(range(10, 19)),
        range(10, 20).exclude(range(15, 50)) == List(range(10, 14)),
        range(10, 20).exclude(range(10, 10)) == List(range(11, 20)),
        range(10, 20).exclude(range(5, 15)) == List(range(16, 20)),
        range(10, 20).exclude(range(13, 17)) == List(range(10, 12), range(18, 20)),
        range(10, 20).exclude(range(30, 40)).isEmpty,
        range(10, 20).intersect(range(15, 25)).contains(range(15, 20)),
        range(10, 20).intersect(range(5, 15)).contains(range(10, 15)),
        range(10, 20).intersect(range(30, 40)).isEmpty,
        range(45, 55).intersect(range(0, 69)).contains(range(45, 55))
      )
    },
    test("star#1") {
      for {
        exampleInput <- fileContent(Path(s"data/$day/example-1.txt"))
        almanac       = parse(exampleInput)
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        almanac.conversions.size == 7,
        almanac.seeds == List(79, 14, 55, 13),
        exampleResult == 35L,
        puzzleResult == 457535844L
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        // puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 46L
        // puzzleResult == 0
      )
    }
  ) @@ timed @@ sequential
}
