import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Sink}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Day4 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  case class Card(Id: Int, winningNumbers: Array[Int], haveNumbers: Array[Int])

  def parseCardData(line: String) = {
    val Array(card, data) = line.split(":").map(_.trim)
    val cardId = card.split(" ").last.toInt
    val Array(winningNumbers, haveNumbers) = data.split("\\|")
      .map(_.trim.split(" +").map(_.toInt))

    Card(cardId, winningNumbers, haveNumbers)
  }

  def runPart1 = {
    val fs = FileSystems.getDefault

    val sink = Sink.fold[Int, Int](0)(_ + _)

    val future = FileIO.fromPath(fs.getPath("../day4/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .map(parseCardData)
      .map {
        case Card(_, winningNumbers, havingNumbers) => Math.pow(2, winningNumbers.intersect(havingNumbers).length - 1).toInt
      }
      .runWith(sink)
    //.runWith(Sink.ignore)

    Await.result(future, Duration.Inf)
  }

  def runPart2 = {
    case class FollowingCopies(followingCopies: List[Int], cardCount: Int)

    val fs = FileSystems.getDefault

    val sink = Sink.last[Int]

    val future = FileIO.fromPath(fs.getPath("../day4/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .map(parseCardData)
      .fold(FollowingCopies(Nil, 0))((accum, card) => {
        val Card(_, winningNumbers, havingNumbers) = card
        val FollowingCopies(followingCopies, cardCount) = accum
        val winCount = winningNumbers.intersect(havingNumbers).length

        accum match {
          case FollowingCopies(Nil, cardCount) => {
            val value = FollowingCopies(List.fill(winCount)(1), cardCount + 1)
            println(value)
            value
          }
          case FollowingCopies(followingCopies, cardCount) => {
            val currentCopies :: nextFollowingCopies = followingCopies
            val newNextFollowingCopies = if winCount > nextFollowingCopies.length then {
              nextFollowingCopies.map(_ + currentCopies + 1).concat(List.fill(winCount - nextFollowingCopies.length)(currentCopies + 1))
            } else {
              nextFollowingCopies.zipWithIndex.map((value, index) => if index < winCount then value + currentCopies + 1 else value)
            }

            val value = FollowingCopies(newNextFollowingCopies, cardCount + currentCopies + 1)
            println(value)
            value
          }
        }
      })
      .map(_.cardCount)
      .runWith(sink)
    //.runWith(Sink.ignore)

    Await.result(future, Duration.Inf)
  }
}
