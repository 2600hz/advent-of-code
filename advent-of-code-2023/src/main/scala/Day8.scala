import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Sink, Source}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Day8 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  case class Data(movements: List[Char], conversionTable: Map[String, (String, String)])

  def runPart1 = {
    val fs = FileSystems.getDefault

    var conversionTable: Map[String, (String, String)] = Map()

    val sink = Sink.head[Long]

    val future = FileIO.fromPath(fs.getPath("../day8/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 512, true))
      .map(_.utf8String)
      .fold(Data(Nil, Map()))((accum, line) => {
        line.trim match {
          case "" => accum
          case line if accum.movements == Nil => Data(line.toList, accum.conversionTable)
          case line => {
            val List(a, b, c) = "[0-9A-Z]+".r.findAllIn(line).toList
            Data(accum.movements, accum.conversionTable + (a -> (b, c)))
          }
        }
      })
      .flatMapConcat(data => {
        conversionTable = data.conversionTable
        Source.repeat(data.movements)
      })
      .flatMapConcat(movements => Source(movements))
      .aggregateWithBoundary[(String, Long), Long](() => ("AAA", 0))(
        (agg, movement) => {
          val (location, count) = agg
          val (left, right) = conversionTable(location)
          val nextLocation = if (movement == 'L') then left else right
          val shouldEnd = nextLocation== "ZZZ"
          ((nextLocation, count + 1), shouldEnd)
        },
        (_, count) => count,
        None
      )
      .runWith(sink)

    Await.result(future, Duration.Inf)
  }

  def runPart2 = {
    def lowerCommonMultiple(list: List[BigInt]): BigInt = list.foldLeft(1: BigInt) {
      (a, b) =>
        b * a /
          LazyList.iterate((a, b)) { case (x, y) => (y, x % y) }.dropWhile(_._2 != 0).head._1.abs
    }

    val fs = FileSystems.getDefault

    var conversionTable: Map[String, (String, String)] = Map()

    val sink = Sink.head[BigInt]

    val future = FileIO.fromPath(fs.getPath("../day8/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 512, true))
      .map(_.utf8String)
      .fold(Data(Nil, Map()))((accum, line) => {
        line.trim match {
          case "" => accum
          case line if accum.movements == Nil => Data(line.toList, accum.conversionTable)
          case line => {
            val List(a, b, c) = "[0-9A-Z]+".r.findAllIn(line).toList
            Data(accum.movements, accum.conversionTable + (a -> (b, c)))
          }
        }
      })
      .flatMapConcat(data => {
        conversionTable = data.conversionTable
        //println(conversionTable)
        Source.repeat(data.movements)
      })
      .flatMapConcat(movements => Source(movements))
      .aggregateWithBoundary[List[(String, BigInt)], BigInt](() => {
        conversionTable.keys.filter(_.endsWith("A")).map((_, 0: BigInt)).toList
      })(
        (agg, movement) => {
          //println(agg)
          //println(movement)
          val nextAgg = agg.map((location, count) => {
            if (location.endsWith("Z")) {
              (location, count)
            } else {
              val (left, right) = conversionTable(location)
              val nextLocation = if (movement == 'L') then left else right
              (nextLocation, count + 1)
            }
          })

          val shouldEnd = nextAgg.forall(_._1.endsWith("Z"))

          (nextAgg, shouldEnd)
        },
        agg => lowerCommonMultiple(agg.map(_._2)),
        None
      )
      .runWith(sink)

    Await.result(future, Duration.Inf)
  }
}
