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

def parse(input: String): (Iterator[Instruction], Relations) =
  input.split("\n\n", 2) match {
    case Array(instructions, relationsRaw) =>
      val relations =
        relationsRaw
          .split("\n")
          .collect { case relationRE(from, left, right) => from -> (left, right) }
          .toMap
      LazyList.continually(LazyList.from(instructions)).flatten.iterator -> relations
  }

// ------------------------------------------------------------------------------

def walk1(current: Key, instructions: Iterator[Instruction], relations: Relations, depth: Int): Int = {
  if (current == "ZZZ") depth
  else {
    val instruction = instructions.next()
    relations.get(current) match {
      case Some((left, right)) if instruction == 'L' => walk1(left, instructions, relations, depth + 1)
      case Some((left, right)) if instruction == 'R' => walk1(right, instructions, relations, depth + 1)
    }
  }
}

def resolveStar1(input: String): Int =
  val (instructions, relations) = parse(input)
  walk1("AAA", instructions, relations, 0)

// ------------------------------------------------------------------------------

//@tailrec
//def walk2prev(currents: Iterable[Key], instructions: Iterator[Instruction], relations: Relations, depth: Int): Int = {
//  if (currents.forall(_.endsWith("Z"))) depth
//  else
//    val nextCurrents = instructions.next() match {
//      case 'L' => currents.flatMap(current => relations.get(current).map(_._1))
//      case 'R' => currents.flatMap(current => relations.get(current).map(_._2))
//    }
//    walk2prev(nextCurrents, instructions, relations, depth + 1)
//}

@tailrec
def walk2(currents: Array[Key], instructions: Iterator[Instruction], relations: Relations, depth: Long): Long = {
  if (depth % 5_000_000L == 0) println(s"""$depth : ${currents.toList.mkString("-")}""")
  if (currents.forall(_.endsWith("Z"))) depth
  else {
    if (instructions.next == 'L')
      for (i <- 0.until(currents.size)) currents.update(i, relations(currents(i))(0))
    else
      for (i <- 0.until(currents.size)) currents.update(i, relations(currents(i))(1))
    walk2(currents, instructions, relations, depth + 1)
  }
}

def resolveStar2(input: String): Long =
  val (instructions, relations) = parse(input)
  val startNodes                = relations.keys.filter(_.endsWith("A"))
  walk2(startNodes.toArray, instructions.iterator, relations, 0L)

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
        puzzleResult == 0L
      )
    }
  ) @@ timed @@ sequential
}
