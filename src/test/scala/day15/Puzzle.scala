package day15

import zio.*
import zio.test.*
import zio.test.TestAspect.*

import scala.annotation.tailrec

// ------------------------------------------------------------------------------
def parse(input: List[String]) =
  input.head.trim.split(",")

// ------------------------------------------------------------------------------

def hash(input: String): Int = {
  @tailrec
  def compute(current: List[Char], hashValue: Int): Int = {
    current match {
      case Nil          => hashValue
      case ch :: remain => compute(remain, ((hashValue + ch.toInt) * 17) % 256)
    }
  }
  compute(input.toList, 0)
}

def resolveStar1(input: List[String]): Int =
  val steps = parse(input)
  steps.map(hash).sum

// ------------------------------------------------------------------------------

case class Step(label: String, operation: '=' | '-', focalLength: Option[Int]) {
  val box2use = hash(label)
}

object Step {
  val StepRE = """(\w+)([-=])(\d*)""".r

  def fromString(input: String): Step = {
    input match {
      case StepRE(label, "=", num) => Step(label, '=', Some(num.toInt))
      case StepRE(label, "-", _)   => Step(label, '-', None)
    }
  }
}

case class Box(id: Int, lensSlots: IndexedSeq[(String, Int)] = IndexedSeq.empty) {
  def indexOf(thatLens: String) = lensSlots.indexWhere((lens, _) => lens == thatLens) // TODO not optimal

  def valueOf(thatLens: String) = lensSlots.collect { case (lens, value) if lens == thatLens => value }.head // TODO not optimal

  def upserted(thatLens: String, focalLength: Int): Box = {
    indexOf(thatLens) match {
      case -1    => copy(lensSlots = lensSlots :+ (thatLens, focalLength))
      case index => copy(lensSlots = lensSlots.updated(index, (thatLens, focalLength)))
    }
  }

  def removed(thatLens: String): Box = {
    copy(lensSlots = lensSlots.filterNot((lens, focal) => lens == thatLens))
  }
}

def processStep(boxes: IndexedSeq[Box], step: Step): IndexedSeq[Box] = {
  val index = step.box2use
  boxes.lift(index) match {
    case None                               => ???
    case Some(box) if step.operation == '=' => boxes.updated(index, box.upserted(step.label, step.focalLength.get))
    case Some(box) if step.operation == '-' => boxes.updated(index, box.removed(step.label))
  }
}

def resolveStar2(input: List[String]): Int = {
  val steps        = parse(input).map(Step.fromString)
  val boxes        = 0.to(255).map(id => Box(id))
  val updatedBoxes = steps.foldLeft(boxes)(processStep)
  val boxByLens    =
    updatedBoxes
      .flatMap(box => box.lensSlots.map((lens, focal) => lens -> box))
      .groupMapReduce((lens, box) => lens)((lens, box) => box)((box1, box2) => box1)
  boxByLens.map((lens, box) => (box.id + 1) * (box.indexOf(lens) + 1) * box.valueOf(lens)).sum
}

// ------------------------------------------------------------------------------

object Puzzle15Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 1320,
        puzzleResult == 510388
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 145,
        puzzleResult == 291774
      )
    }
  ) @@ timed @@ sequential
}
