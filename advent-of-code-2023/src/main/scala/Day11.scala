import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Sink, Source}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Day11 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  case class Position(x: Long, y: Long)

  def runPart1 = {
    val fs = FileSystems.getDefault

    // Hello recursion my old friend. Made it tail.
    def getShortestIndexDistancesSum(indexes: List[Long], length: Long, currentListIndex: Int = 1, sum: Long = 0): Long = {
      indexes match {
        case index :: Nil => sum
        case index :: rest => {
          val minDistanceBetweenPairsSum = rest.zipWithIndex.map((otherIndex, listIndex) => {
            val verticalDistance = Math.abs((otherIndex / length) - (index / length))
            val horizontalDistance = Math.abs((otherIndex % length) - (index % length))
            val distance = verticalDistance + horizontalDistance

            //println(s"Distance between $currentListIndex and ${currentListIndex + listIndex + 1} is $distance")

            distance
          }).sum

          getShortestIndexDistancesSum(rest, length, currentListIndex + 1, sum + minDistanceBetweenPairsSum)
        }
      }
    }

    val future = FileIO.fromPath(fs.getPath("../day11/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 512, true))
      .map(_.utf8String)
      .fold((0, 0, List[Position]()))((accum, line) => {
        val (currentY, _, positions) = accum
        val newPositions = "#".r.findAllMatchIn(line).map(m => Position(m.start, currentY)).toList
        (currentY + 1, line.length, positions.concat(newPositions))
      })
      .map {
        case (_, lineLength, positions) => {
          val allX = positions.map(p => p.x).distinct
          val allY = positions.map(p => p.y).distinct
          val newLineLength = lineLength * 2 - allX.length

          //println(s"New line length is $newLineLength")

          val recalculatedPositionsAsIndexes = positions.zipWithIndex.map((p, i) => {
            //println(p)
            val subX = allX.count(x => x < p.x)
            val subY = allY.count(y => y < p.y)

            val newP = Position(p.x*2 - subX, p.y*2 - subY)

            // Getting fancy by converting position to index as if the whole file were a single string with no line breaks
            // Note that they will already be sorted from lower to higher
            val indexedPosition = (newP.y * newLineLength) + newP.x

            //println((i + 1, newP, indexedPosition))

            indexedPosition
          })

          getShortestIndexDistancesSum(recalculatedPositionsAsIndexes, newLineLength)
        }
      }
      .runWith(Sink.head[Long])

    Await.result(future, Duration.Inf)
  }

  def runPart2 = {
    val emptySpaceMultiplier = 1000000

    val fs = FileSystems.getDefault

    case class PointsAndDistance(p1: Position, p2: Position, distanceX: Long, distanceY: Long)

    // Hello recursion my old friend. Made it tail.
    def getShortestDistances(positions: List[Position], currentIndex: Int = 0, result: List[PointsAndDistance] = List()): List[PointsAndDistance] = {
      positions match {
        case position :: Nil => result
        case position :: rest => {
          val distancesBetweenPositions = rest.zipWithIndex.map((otherPosition, listIndex) => {
            val distanceY = Math.abs(otherPosition.y - position.y)
            val distanceX = Math.abs(otherPosition.x - position.x)

            val pd = PointsAndDistance(position, otherPosition, distanceX, distanceY)

            //println(s"Distance between ${currentIndex + 1} and ${listIndex + currentIndex + 2} is $pd")

            pd
          })

          getShortestDistances(rest, currentIndex + 1, result.concat(distancesBetweenPositions))
        }
      }
    }

    val sink = Sink.fold[BigInt, BigInt](BigInt(0))(_ + _)

    val future = FileIO.fromPath(fs.getPath("../day11/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 512, true))
      .map(_.utf8String)
      .fold((0, 0, List[Position]()))((accum, line) => {
        val (currentY, _, positions) = accum
        val newPositions = "#".r.findAllMatchIn(line).map(m => Position(m.start, currentY)).toList
        (currentY + 1, line.length, positions.concat(newPositions))
      })
      .flatMapConcat {
        case (_, lineLength, positions) => {
          val allX = positions.map(p => p.x).distinct
          val allY = positions.map(p => p.y).distinct

          //println(s"AllX are $allX")
          //println(s"AllY are $allY")

          val positionAndDistances = getShortestDistances(positions)

          val distances = positionAndDistances.map {
            case PointsAndDistance(p1, p2, distanceX, distanceY) => {
              val minX = Math.min(p1.x, p2.x)
              val maxX = Math.max(p1.x, p2.x)
              val minY = Math.min(p1.y, p2.y)
              val maxY = Math.max(p1.y, p2.y)

              val starsBetweenX = allX.count(x => minX < x && x < maxX)
              val starsBetweenY = allY.count(y => minY < y && y < maxY)

              val emptyColumnsBetweenX = if (distanceX == 0) then 0 else distanceX - starsBetweenX - 1
              val emptyRowsBetweenY = if (distanceY == 0) then 0 else distanceY - starsBetweenY - 1

              //println(s"Between $p1 and $p2 are $emptyColumnsBetweenX emptyX and $emptyRowsBetweenY emptyY")

              val expandedDistanceX = BigInt(distanceX) + (BigInt(emptySpaceMultiplier - 1) * BigInt(emptyColumnsBetweenX))
              val expandedDistanceY = BigInt(distanceY) + (BigInt(emptySpaceMultiplier - 1) * BigInt(emptyRowsBetweenY))

              expandedDistanceX + expandedDistanceY
            }
          }

          Source(distances)
        }
      }
      .runWith(sink)

    Await.result(future, Duration.Inf)
  }
}
