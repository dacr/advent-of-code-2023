package day06

import zio.*
import zio.test.*
import zio.test.TestAspect.*
case class Race(maxTime: Long, currentRecord: Long)
// ------------------------------------------------------------------------------
val lineRE                                   = """\w+:\s+([0-9 ]+)""".r
def numbersToList(input: String): List[Long] = input.trim.split("\\s+").map(_.toLong).toList
def parseStar1(input: List[String])          =
  input match {
    case lineRE(timesRaw) :: lineRE(recordsRaw) :: Nil =>
      val maxTimes = numbersToList(timesRaw)
      val records  = numbersToList(recordsRaw)
      maxTimes.zip(records).map((maxTime, distance) => Race(maxTime, distance))
  }

// ------------------------------------------------------------------------------
def getDistance(waitTime: Long, maxTime: Long): Long = math.max(0, waitTime * (maxTime - waitTime))

def resolveStar1(input: List[String]): Long =
  val races = parseStar1(input)
  races
    .map(race =>
      0.until(race.maxTime.toInt)
        .count(waitTime => getDistance(waitTime, race.maxTime) > race.currentRecord)
    )
    .reduce(_ * _)

// ------------------------------------------------------------------------------

def parseStar2(input: List[String]) =
  input match {
    case lineRE(timesRaw) :: lineRE(recordsRaw) :: Nil =>
      val maxTime = timesRaw.replaceAll(" ", "").toLong
      val record  = recordsRaw.replaceAll(" ", "").toLong
      Race(maxTime, record)
  }

def findFirst(race: Race): Long = {
  def worker(leftWaitTime: Long, currentWaitTime: Long, rightWaitTime: Long): Long = {
    //println(s"first : $leftWaitTime, $currentWaitTime, $rightWaitTime")
    if (currentWaitTime == leftWaitTime) currentWaitTime
    else {
      if (getDistance(currentWaitTime, race.maxTime) > race.currentRecord)
        worker(leftWaitTime, leftWaitTime + (currentWaitTime - leftWaitTime) / 2, currentWaitTime)
      else
        worker(currentWaitTime, currentWaitTime + (rightWaitTime - currentWaitTime) / 2, rightWaitTime)
    }
  }
  worker(0L, race.maxTime / 4, race.maxTime / 2)
}

def findLast(race: Race): Long = {
  def worker(leftWaitTime: Long, currentWaitTime: Long, rightWaitTime: Long): Long = {
    //println(s"last: $leftWaitTime, $currentWaitTime, $rightWaitTime")
    if (currentWaitTime == leftWaitTime) currentWaitTime
    else {
      if (getDistance(currentWaitTime, race.maxTime) < race.currentRecord)
        worker(leftWaitTime, leftWaitTime + (currentWaitTime - leftWaitTime) / 2, currentWaitTime)
      else
        worker(currentWaitTime, currentWaitTime + (rightWaitTime - currentWaitTime) / 2, rightWaitTime)
    }
  }
  worker(race.maxTime / 2, race.maxTime / 2 + race.maxTime / 4, race.maxTime)
}

def resolveStar2(input: List[String]): Long =
  val race = parseStar2(input)
  findLast(race) - findFirst(race)

// ------------------------------------------------------------------------------

object Puzzle06Test extends ZIOSpecDefault {
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
        exampleResult == 288,
        puzzleResult == 227850
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 71503,
        puzzleResult == 42948149
      )
    }
  ) @@ timed @@ sequential
}
