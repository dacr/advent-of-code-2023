package day07

import zio.*
import zio.test.*
import zio.test.TestAspect.*

// ------------------------------------------------------------------------------
object Day07Star1 {
  val strengths =
    List('2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A').zipWithIndex
      .map((card, index) => (card, index + 1))
      .toMap

  enum HandType(val strength: Long) {
    case FiveOfAKind  extends HandType(7_000_000_000L)
    case FourOfAKind  extends HandType(6_000_000_000L)
    case FullHouse    extends HandType(5_000_000_000L)
    case ThreeOfAKind extends HandType(4_000_000_000L)
    case TwoPairs     extends HandType(3_000_000_000L)
    case OnePair      extends HandType(2_000_000_000L)
    case HighCard     extends HandType(1_000_000_000L)
  }

  object HandType {
    def fromCards(hand: String): HandType = {
      val groups       = hand.groupBy(ch => ch)
      val groupMaxSize = groups.values.map(_.size).max
      groups.size match {
        case 1                      => FiveOfAKind
        case 2 if groupMaxSize == 4 => FourOfAKind
        case 2                      => FullHouse
        case 3 if groupMaxSize == 3 => ThreeOfAKind
        case 3                      => TwoPairs
        case 4                      => OnePair
        case 5                      => HighCard
      }
    }
  }

  case class Hand(cards: String, handType: HandType, value: Long)

  case class Bid(hand: Hand, amount: Int)

  def cardsValue(cards: String): Long = {
    4.to(0, -1).zip(cards).map((pwr, card) => strengths(card) * math.pow(13, pwr).toLong).sum
  }

  // ------------------------------------------------------------------------------
  def parse(input: List[String]): List[Bid] =
    input.map(line =>
      line.trim.split("\\s+") match {
        case Array(cards, bidRaw) =>
          val handType  = HandType.fromCards(cards)
          val handValue = handType.strength + cardsValue(cards)
          val hand      = Hand(cards, handType, handValue)
          Bid(hand, bidRaw.toInt)
      }
    )

  // ------------------------------------------------------------------------------

  def resolveStar1(input: List[String]): Int =
    val bids = parse(input)
    bids
      .sortBy(_.hand.value)
      .zipWithIndex
      .map((bid, index) => bid.amount * (index + 1))
      .sum
}
// ------------------------------------------------------------------------------

object Day07Star2 {

  val strengths =
    List('J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A').zipWithIndex
      .map((card, index) => (card, index + 1))
      .toMap

  enum HandType(val strength: Int) {
    case FiveOfAKind  extends HandType(7)
    case FourOfAKind  extends HandType(6)
    case FullHouse    extends HandType(5)
    case ThreeOfAKind extends HandType(4)
    case TwoPairs     extends HandType(3)
    case OnePair      extends HandType(2)
    case HighCard     extends HandType(1)
  }

  object HandType {
    def fromCards(hand: String): HandType = {
      val allCounts   = strengths.keys.map(card => card -> hand.count(_ == card)).toList
      val cardsCounts = allCounts.collect { case (card, count) if count > 0 && card != 'J' => count }.sorted.reverse
      cardsCounts match {
        case l if l.size <= 1             => FiveOfAKind
        case 1 :: Nil                     => FourOfAKind
        case _ :: 1 :: Nil                => FourOfAKind
        case 2 :: Nil                     => FullHouse
        case _ :: 2 :: Nil                => FullHouse
        case 1 :: 1 :: Nil                => ThreeOfAKind
        case _ :: 1 :: 1 :: Nil           => ThreeOfAKind
        case _ :: 2 :: 1 :: Nil           => TwoPairs
        case _ :: 1 :: 1 :: 1 :: Nil      => OnePair
        case 1 :: 1 :: 1 :: 1 :: 1 :: Nil => HighCard
      }
    }
  }

  case class Hand(cards: String, handType: HandType, value: Long)

  case class Bid(hand: Hand, amount: Int)

  def cardsValue(cards: String): Long = {
    4.to(0, -1).zip(cards).map((pwr, card) => strengths(card) * math.pow(13, pwr).toLong).sum
  }

  // ------------------------------------------------------------------------------
  def parse(input: List[String]): List[Bid] =
    input.map(line =>
      line.trim.split("\\s+") match {
        case Array(cards, bidRaw) =>
          val handType  = HandType.fromCards(cards)
          val handValue = cardsValue(cards)
          val hand      = Hand(cards, handType, handValue)
          Bid(hand, bidRaw.toInt)
      }
    )

  def resolveStar2(input: List[String]): Int =
    val bids = parse(input)
    bids
      .groupBy(_.hand.handType)
      .view
      .mapValues(bids => bids.sortBy(_.hand.value))
      .toList
      .sortBy((handType, bids) => handType.strength)
      .flatMap((handType, bids) => bids)
      .zipWithIndex
      .map((bid, index) => bid.amount * (index + 1))
      .sum
}

// ------------------------------------------------------------------------------

object Puzzle07Test extends ZIOSpecDefault {
  import zio.nio.file.Path
  import helpers.Helpers.*
  val day  = getClass.getName.replaceAll(""".*Puzzle(\d+)Test.*""", "day$1")
  def spec = suite(s"puzzle $day")(
    test("star#1") {
      for {
        exampleInput <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult = Day07Star1.resolveStar1(exampleInput)
        puzzleInput  <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult  = Day07Star1.resolveStar1(puzzleInput)
      } yield assertTrue(
        exampleResult == 6440,
        puzzleResult == 247823654
      )
    },
    test("star#2") {
      for {
        exampleInput1 <- fileLines(Path(s"data/$day/example-1.txt"))
        exampleResult1 = Day07Star2.resolveStar2(exampleInput1)
        puzzleInput   <- fileLines(Path(s"data/$day/puzzle-1.txt"))
        puzzleResult   = Day07Star2.resolveStar2(puzzleInput)
      } yield assertTrue(
        exampleResult1 == 5905,
        puzzleResult == 245461700
      )
    }
  ) @@ timed @@ sequential
}
