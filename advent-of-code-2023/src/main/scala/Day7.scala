import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Sink, Source}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Day7 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  case class Hand(cards: String, bid: Long, weight: Long)

  val handGroupsSortedByWeightDesc = List(
    List(5),            // Five of a kind
    List(4, 1),         // Four of a kind
    List(3, 2),         // Full house
    List(3, 1, 1),      // Three of a kind
    List(2, 2, 1),      // Two pair
    List(2, 1, 1 ,1),   // One pair
    List(1, 1, 1, 1, 1) // Nothing
  )

  def calculateHandWeight(cards: String) = {
    val hand = cards.groupBy((card: Char) => card).toList.map((card, cards) => cards.length).sortBy(-_)
    handGroupsSortedByWeightDesc.indexWhere(expectedHand => expectedHand.diff(hand).isEmpty).toLong
  }

  def calculateCardsWeight(cardSortedByWeightDesc: String, cards: String) = {
    cards.foldLeft(0L)((accum, card) => (accum * 100) + cardSortedByWeightDesc.indexOf(card).toLong)
  }

  def runPart1 = {
    val cardSortedByWeightDesc = "AKQJT98765432"

    val fs = FileSystems.getDefault

    val sink = Sink.fold[Long, Long](1)(_ * _)

    val future = FileIO.fromPath(fs.getPath("../day7/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .map(line => {
        val List(cards, bidString) = line.split("\\ +").toList
        val bid = bidString.toLong
        val handWeight = calculateHandWeight(cards)
        val cardsWeight = calculateCardsWeight(cardSortedByWeightDesc, cards)
        val weight = (handWeight * Math.pow(100, cards.length).toLong) + cardsWeight
        Hand(cards, bid, weight)
      })
      .runWith(Sink.seq)
    //.runWith(Sink.ignore)

    val hands = Await.result(future, Duration.Inf)
    val sortedHands = hands.sortBy(- _.weight)

    //println(sortedHands)

    sortedHands.zipWithIndex.map((hand, index) => hand.bid * (index + 1)).sum
  }

  /**
   * Get dard permutations. I done this because I thought that the permutations will be also used to get the card weights,
   * but it turns out that the card weights needed to be obtained from the original cards. Being not the case, this could
   * be replaced by combinations, as the card order is no longer important.
   * @param cardSet Set of cards to be used to replace the J.
   * @param cards   Current set of cards where the J will be replaced.
   * @return        All the possible permutations that can be generated by replacing the J with any cards of the set.
   */
  def getPermutations(cardSet: String, cards: String): List[String] = {
    cards.toList match {
      case Nil => List("")
      case 'J' :: rest => {
        val restPermutations = getPermutations(cardSet, rest.mkString)
        cardSet.flatMap(card => restPermutations.map(cardRest => s"${card}${cardRest}")).toList
      }
      case card :: rest => {
        val restPermutations = getPermutations(cardSet, rest.mkString)
        restPermutations.map(cardRest => s"${card}${cardRest}")
      }
    }
  }

  def runPart2 = {
    val cardSortedByWeightDesc = "AKQT98765432J"

    val fs = FileSystems.getDefault

    val sink = Sink.fold[Long, Long](1)(_ * _)

    val future = FileIO.fromPath(fs.getPath("../day7/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .map(line => {
        val List(cards, bidString) = line.split("\\ +").toList
        val bid = bidString.toLong
        val cardPermutations = getPermutations(cards.toList.distinct.mkString, cards)
        val minHandWeight = cardPermutations.map(x => {
          val handWeight = calculateHandWeight(x)
          val cardsWeight = calculateCardsWeight(cardSortedByWeightDesc, cards)
          (handWeight * Math.pow(100, cards.length).toLong) + cardsWeight
        }).min
        Hand(cards, bid, minHandWeight)
      })
      .runWith(Sink.seq)
    //.runWith(Sink.ignore)

    val hands = Await.result(future, Duration.Inf)
    val sortedHands = hands.sortBy(-_.weight)

    //println(sortedHands)

    sortedHands.zipWithIndex.map((hand, index) => hand.bid * (index + 1)).sum
  }
}
