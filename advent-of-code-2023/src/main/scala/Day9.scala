import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Sink, Source}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Day9 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  def calculateNextValue(values: List[BigInt]): BigInt = {
    values match {
      case Nil => 0
      case values if values.forall(_ == 0) => 0
      case values => {
        val diffValues = values.sliding(2, 1).map {
          case List(a, b) => b - a
        }.toList
        //println(diffValues)
        values.last + calculateNextValue(diffValues)
      }
    }
  }

  def calculatePreviousValue(values: List[BigInt]): BigInt = {
    values match {
      case Nil => 0
      case values if values.forall(_ == 0) => 0
      case values => {
        val diffValues = values.sliding(2, 1).map {
          case List(a, b) => b - a
        }.toList
        values.head - calculatePreviousValue(diffValues)
      }
    }
  }

  def runPart1 = {
    val fs = FileSystems.getDefault

    var conversionTable: Map[String, (String, String)] = Map()

    val sink = Sink.fold[BigInt, BigInt](0)(_ + _)

    val future = FileIO.fromPath(fs.getPath("../day9/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 512, true))
      .map(_.utf8String)
      .map(_.split("\\ +").map(BigInt(_)).toList)
      .map(calculateNextValue(_))
      .runWith(sink)

    Await.result(future, Duration.Inf)
  }

  def runPart2 = {
    val fs = FileSystems.getDefault

    var conversionTable: Map[String, (String, String)] = Map()

    val sink = Sink.fold[BigInt, BigInt](0)(_ + _)

    val future = FileIO.fromPath(fs.getPath("../day9/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 512, true))
      .map(_.utf8String)
      .map(_.split("\\ +").map(BigInt(_)).toList)
      .map(calculatePreviousValue(_))
      .runWith(sink)

    Await.result(future, Duration.Inf)
  }
}
