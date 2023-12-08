package day08

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec

type Instruction = Char
type Key         = String
type Relations   = Map[Key, (Key, Key)]
// ------------------------------------------------------------------------------
val relationRE = """(\w+) *= *\((\w+), *(\w+)\)""".r

def parse(input: String): ( () => Iterator[Instruction], Relations) =
  input.split("\n\n", 2) match {
    case Array(instructions, relationsRaw) =>
      val relations =
        relationsRaw
          .split("\n")
          .collect { case relationRE(from, left, right) => from -> (left, right) }
          .toMap
      ( () => LazyList.continually(LazyList.from(instructions)).flatten.iterator) -> relations
  }

// ------------------------------------------------------------------------------

@tailrec
def walk(current: Key, instructions: Iterator[Instruction], relations: Relations, depth: Int): Int = {
  if (current == "ZZZ") depth
  else {
    val instruction = instructions.next()
    relations.get(current) match {
      case Some((left, right)) if instruction == 'L' => walk(left, instructions, relations, depth + 1)
      case Some((left, right)) if instruction == 'R' => walk(right, instructions, relations, depth + 1)
    }
  }
}

def resolveStar1(input: String): Int =
  val (instructions, relations) = parse(input)
  walk("AAA", instructions(), relations, 0)

// ------------------------------------------------------------------------------
def gcd(a: Long, b: Long): Long = if (b == 0) a else gcd(b, a % b)
def lcm(a: Long, b: Long): Long = if (a == 0 || b == 0) 0 else a * b / gcd(a, b)
def gcds(nums: Iterable[Long]): Long = nums.reduce(gcd)
def lcms(nums: Iterable[Long]): Long = nums.reduce(lcm)

@tailrec
def walk2(current: Key, instructions: Iterator[Instruction], relations: Relations, depth: Int): Int = {
  if (current.endsWith("Z")) depth
  else {
    val instruction = instructions.next()
    relations.get(current) match {
      case Some((left, right)) if instruction == 'L' => walk2(left, instructions, relations, depth + 1)
      case Some((left, right)) if instruction == 'R' => walk2(right, instructions, relations, depth + 1)
    }
  }
}

def resolveStar2(input: String): Long =
  val (instructions, relations) = parse(input)
  val startNodes                = relations.keys.filter(_.endsWith("A"))
  val frequencies = startNodes.map(current => walk2(current, instructions(), relations, 0).toLong)
  lcms(frequencies)


// ------------------------------------------------------------------------------

object Puzzle08Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar1(exampleInput1)
        exampleInput2 <- fileContent(Path(s"data/$day/example-2.txt"))
        exampleResult2 = resolveStar1(exampleInput2)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 2,
        exampleResult2 == 6,
        puzzleResult == 18023
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileContent(Path(s"data/$day/example-3.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileContent(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 6L,
        puzzleResult > 101149L,
        puzzleResult == 14449445933179L
      )
    }
  ) @@ timed @@ sequential
}
