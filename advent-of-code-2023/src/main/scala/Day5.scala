import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Sink}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Day5 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  case class ConversionMap(from: String, to: String, start: Long, end: Long, add: Long)

  case class Data(seeds: List[Long], conversionTable: List[ConversionMap], nextFrom: String, nextTo: String)

  def readData = {
    val fs = FileSystems.getDefault

    val sink = Sink.last[Data]

    val future = FileIO.fromPath(fs.getPath("../day5/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .fold(Data(Nil, Nil, "", ""))((accum, line) => {
        line.split("[\\ -]").toList match {
          case Nil => accum
          case "seeds:" :: seedList => Data(seedList.map(_.toLong), accum.conversionTable, "", "")
          case source :: "to" :: destination :: _ => Data(accum.seeds, accum.conversionTable, source, destination)
          case destinationRangeStart :: sourceRangeStart :: rangeLength :: _ => {
            Data(
              accum.seeds,
              accum.conversionTable :+ ConversionMap(accum.nextFrom, accum.nextTo, sourceRangeStart.toLong, sourceRangeStart.toLong + rangeLength.toLong - 1, destinationRangeStart.toLong - sourceRangeStart.toLong),
              accum.nextFrom,
              accum.nextTo
            )
          }
          case _ => accum
        }
      })
      .runWith(sink)

    Await.result(future, Duration.Inf)
  }

  def runPart1 = {
    val data = readData

    def getLocationFrom(value: Long, from: String): Long = {
      from match {
        case "location" => value
        case from => {
          val conversion = data.conversionTable.find(x => x.from == from && x.start <= value && x.end >= value)

          conversion match {
            case None => {
              val conversion = data.conversionTable.find(x => x.from == from).head

              getLocationFrom(value, conversion.to)
            }
            case Some(ConversionMap(_, to, _, _, add)) => getLocationFrom(value + add, to)
          }
        }
      }
    }

    data.seeds.map(seed => getLocationFrom(seed, "seed")).min
  }

  def runPart2 = {
    case class Range(start: Long, end: Long)

    val data = readData

    def getMinLocationFrom(range: Range, from: String): Long = {
      from match {
        case "location" => range.start
        case from => {
          val conversionsFromSource = data.conversionTable.filter(x => x.from == from)
          val conversionsWithinRange = conversionsFromSource.filter(x =>
            (x.start >= range.start && x.start <= range.end)
              || (x.end >= range.start && x.end <= range.end)
              || (range.start >= x.start && range.start <= x.end)
              || (range.end >= x.start && range.end <= x.end))
          val potentialBreakpoints = conversionsWithinRange
            // Get breakpoints
            .flatMap(x => List(x.start, x.end + 1))
            // Concat range start and end, to add as breakpoints
            .concat(List(range.start, range.end + 1))
            // Sort
            .sorted
            // Remove repeated
            .distinct

          // Remove potential out of range
          val breakpoints = potentialBreakpoints.filter(x => x >= range.start && x <= range.end + 1)

          //println(range)
          //println(conversionsFromSource)
          //println(conversionsWithinRange)
          //println(potentialBreakpoints)
          //println(breakpoints)

          val newRanges = breakpoints.sliding(2, 1).map {
            case start::endPlusOne::_ => Range(start, endPlusOne - 1)
          }.toList

          //println(newRanges)

          newRanges.map(newRange => {
            val conversion = data.conversionTable.find(x => x.from == from && x.start <= newRange.start && x.end >= newRange.start)

            conversion match {
              case None => {
                val conversion = data.conversionTable.find(x => x.from == from).head

                getMinLocationFrom(newRange, conversion.to)
              }
              case Some(ConversionMap(_, to, _, _, add)) => getMinLocationFrom(Range(newRange.start + add, newRange.end + add), to)
            }
          }).min
        }
      }
    }

    val seeds = data.seeds.grouped(2).map {
      case a :: b :: _ => Range(a, a + b - 1)
    }

    seeds.map(seed => getMinLocationFrom(seed, "seed")).min
  }
}
