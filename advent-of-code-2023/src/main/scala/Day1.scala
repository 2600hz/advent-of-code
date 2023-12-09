import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Sink}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.postfixOps

object Day1 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  def runPart1 = {
    val fs = FileSystems.getDefault
    
    val sink = Sink.fold[Int, Int](0)(_ + _)

    val future = FileIO.fromPath(fs.getPath("../day1/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .map[Int](line =>
        (line.find(_.isDigit), line.findLast(_.isDigit)) match {
            case (Some(x), Some(y)) => s"${x}${y}".toInt
            case _ => 0
        }
      ).runWith(sink)

    Await.result(future, Duration.Inf)
  }

  def runPart2 = {
    def findWrittenNumber(line: String): Option[Int] = {
        List("zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine").indexWhere(line.startsWith(_)) match {
        case index if index < 0 => None
        case index => Some(index)
        }
    }

    def findDigitFromStart(line: String): Option[Int] = {
        line match {
        case "" => None
        case line if line.head.isDigit => Some(line.head.toInt - 48)
        case line => findWrittenNumber(line) match {
            case Some(x) => Some(x)
            case None => findDigitFromStart(line.substring(1))
        }
        }
    }

    def findDigitFromEnd(line: String): Option[Int] = {
        line match
        case "" => None
        case line => findDigitFromEnd(line.substring(1)) match {
            case Some(x) => Some(x)
            case None if line.head.isDigit => Some(line.head.toInt - 48)
            case _ => findWrittenNumber(line)
        }
    }

    val fs = FileSystems.getDefault

    val sink = Sink.fold[Int, Int](0)(_ + _)

    val future = FileIO.fromPath(fs.getPath("../day1/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .map[Int](line =>
        (findDigitFromStart(line), findDigitFromEnd(line)) match {
          case (Some(x), Some(y)) => s"${x}${y}".toInt
          case _ => 0
        }
      )
      .runWith(sink)

    Await.result(future, Duration.Inf)
  }
}
