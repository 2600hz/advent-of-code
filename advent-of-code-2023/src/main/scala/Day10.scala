import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Flow, Framing, Merge, Sink, Source}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}

object Day10 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  trait PositionTrait {
    def x: Int

    def y: Int
  }

  case class Position(x: Int, y: Int) extends PositionTrait

  case class MoveEast(x: Int, y: Int) extends PositionTrait

  case class MoveWest(x: Int, y: Int) extends PositionTrait

  case class MoveNorth(x: Int, y: Int) extends PositionTrait

  case class MoveSouth(x: Int, y: Int) extends PositionTrait

  def getMatrixPositionCurried(lineLength: Int)(index: Int) = Position(index % lineLength, index / lineLength)

  def getIndexCurried(lineLength: Int)(position: PositionTrait) = (position.y * lineLength) + position.x

  def moveToEast(p: PositionTrait) = MoveEast(p.x + 1, p.y)

  def moveToWest(p: PositionTrait) = MoveWest(p.x - 1, p.y)

  def moveToNorth(p: PositionTrait) = MoveNorth(p.x, p.y - 1)

  def moveToSouth(p: PositionTrait) = MoveSouth(p.x, p.y + 1)

  case class MovementState(movement: PositionTrait, count: Long)

  case class MovementStateWithPath(movement: PositionTrait, path: List[PositionTrait], count: Long)

  def runPart1 = {
    val fs = FileSystems.getDefault

    var conversionTable: Map[String, (String, String)] = Map()

    val sink = Sink.fold[Long, Long](0L)((a, b) => Math.max(a, b/2L))

    var getCharInPosition: (p: PositionTrait) => Try[Char] = (p: PositionTrait) => Failure(NotImplementedError())

    val future = FileIO.fromPath(fs.getPath("../day10/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 512, true))
      .map(_.utf8String)
      .fold((0, new StringBuilder()))((accum, line) => (line.length, accum._2.append(line)))
      .flatMapConcat((lineLength, sb) => {
        val fullMaze = sb.mkString
        val getMatrixPosition = getMatrixPositionCurried(lineLength)
        val getIndex = getIndexCurried(lineLength)
        val startPosition = getMatrixPosition(fullMaze.indexOf('S'))

        //println(startPosition)

        def getCharInPosition(p: PositionTrait) = Try(fullMaze(getIndex(p)))

        val repeatingSource = Source.repeat(1)

        val sources = List(
          moveToEast(startPosition),
          moveToWest(startPosition),
          moveToNorth(startPosition),
          moveToSouth(startPosition)
        ).map(initialMovement => {
          val source: Source[Long, NotUsed] = repeatingSource
            .aggregateWithBoundary(() => MovementState(initialMovement, 0L))(
              (agg, _) => {
                val MovementState(movement, count) = agg
                val pipe = getCharInPosition(movement)
                //println(movement)
                //println(pipe)
                val result = movement match {
                  case p: MoveEast => {
                    pipe match {
                      case Success('S') => (MovementState(p, count + 1), true)
                      case Success('-') => (MovementState(moveToEast(p), count + 1), false)
                      case Success('J') => (MovementState(moveToNorth(p), count + 1), false)
                      case Success('7') => (MovementState(moveToSouth(p), count + 1), false)
                      case _ => (MovementState(p, count), true)
                    }
                  }
                  case p: MoveWest => {
                    pipe match {
                      case Success('S') => (MovementState(p, count + 1), true)
                      case Success('-') => (MovementState(moveToWest(p), count + 1), false)
                      case Success('F') => (MovementState(moveToSouth(p), count + 1), false)
                      case Success('L') => (MovementState(moveToNorth(p), count + 1), false)
                      case _ => (MovementState(p, count), true)
                    }
                  }
                  case p: MoveNorth => {
                    pipe match {
                      case Success('S') => (MovementState(p, count + 1), true)
                      case Success('|') => (MovementState(moveToNorth(p), count + 1), false)
                      case Success('F') => (MovementState(moveToEast(p), count + 1), false)
                      case Success('7') => (MovementState(moveToWest(p), count + 1), false)
                      case _ => (MovementState(p, count), true)
                    }
                  }
                  case p: MoveSouth => {
                    pipe match {
                      case Success('S') => (MovementState(p, count + 1), true)
                      case Success('|') => (MovementState(moveToSouth(p), count + 1), false)
                      case Success('L') => (MovementState(moveToEast(p), count + 1), false)
                      case Success('J') => (MovementState(moveToWest(p), count + 1), false)
                      case _ => (MovementState(p, count), true)
                    }
                  }
                  case _ => (MovementState(movement, count), true)
                }

                //println(result)

                result
              },
              agg => agg.count,
              None
            )
            .take(1)

          source
        })

        // FIXME: Gets all the paths, but does not check whether or not ended at the beginning
        Source.combine(sources(0), sources(1), sources(2), sources(3))(Merge(_))
        //sources(0)
      })
      .runWith(sink)

    Await.result(future, Duration.Inf)
  }

  def runPart2 = {
    val fs = FileSystems.getDefault

    var conversionTable: Map[String, (String, String)] = Map()

    val sink = Sink.fold[List[PositionTrait], List[PositionTrait]](List())((a, b) => if a.length > b.length then a else b)

    var getCharInPosition: (p: PositionTrait) => Try[Char] = (p: PositionTrait) => Failure(NotImplementedError())

    val future = FileIO.fromPath(fs.getPath("../day10/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 512, true))
      .map(_.utf8String)
      .fold((0, new StringBuilder()))((accum, line) => (line.length, accum._2.append(line)))
      .flatMapConcat((lineLength, sb) => {
        val fullMaze = sb.mkString
        val getMatrixPosition = getMatrixPositionCurried(lineLength)
        val getIndex = getIndexCurried(lineLength)
        val startPosition = getMatrixPosition(fullMaze.indexOf('S'))

        //println(startPosition)

        getCharInPosition = (p: PositionTrait) => Try(fullMaze(getIndex(p)))

        val repeatingSource = Source.repeat(1)

        val sources = List(
          moveToEast(startPosition),
          moveToWest(startPosition),
          moveToNorth(startPosition),
          moveToSouth(startPosition)
        ).map(initialMovement => {
          val source: Source[List[PositionTrait], NotUsed] = repeatingSource
            .aggregateWithBoundary(() => MovementStateWithPath(initialMovement, List(initialMovement), 0L))(
              (agg, _) => {
                val MovementStateWithPath(movement, path, count) = agg
                val pipe = getCharInPosition(movement)
                //println(movement)
                //println(pipe)
                val result = movement match {
                  case p: MoveEast => {
                    pipe match {
                      case Success('S') => (MovementStateWithPath(p, path, count + 1), true)
                      case Success('-') => (MovementStateWithPath(moveToEast(p), path :+ moveToEast(p), count + 1), false)
                      case Success('J') => (MovementStateWithPath(moveToNorth(p), path :+ moveToNorth(p), count + 1), false)
                      case Success('7') => (MovementStateWithPath(moveToSouth(p), path :+ moveToSouth(p), count + 1), false)
                      case _ => (MovementStateWithPath(p, path, count), true)
                    }
                  }
                  case p: MoveWest => {
                    pipe match {
                      case Success('S') => (MovementStateWithPath(p, path, count + 1), true)
                      case Success('-') => (MovementStateWithPath(moveToWest(p), path :+ moveToWest(p), count + 1), false)
                      case Success('F') => (MovementStateWithPath(moveToSouth(p), path :+ moveToSouth(p), count + 1), false)
                      case Success('L') => (MovementStateWithPath(moveToNorth(p), path :+ moveToNorth(p), count + 1), false)
                      case _ => (MovementStateWithPath(p, path, count), true)
                    }
                  }
                  case p: MoveNorth => {
                    pipe match {
                      case Success('S') => (MovementStateWithPath(p, path, count + 1), true)
                      case Success('|') => (MovementStateWithPath(moveToNorth(p), path :+ moveToNorth(p), count + 1), false)
                      case Success('F') => (MovementStateWithPath(moveToEast(p), path :+ moveToEast(p), count + 1), false)
                      case Success('7') => (MovementStateWithPath(moveToWest(p), path :+ moveToWest(p), count + 1), false)
                      case _ => (MovementStateWithPath(p, path, count), true)
                    }
                  }
                  case p: MoveSouth => {
                    pipe match {
                      case Success('S') => (MovementStateWithPath(p, path, count + 1), true)
                      case Success('|') => (MovementStateWithPath(moveToSouth(p), path :+ moveToSouth(p), count + 1), false)
                      case Success('L') => (MovementStateWithPath(moveToEast(p), path :+ moveToEast(p), count + 1), false)
                      case Success('J') => (MovementStateWithPath(moveToWest(p), path :+ moveToWest(p), count + 1), false)
                      case _ => (MovementStateWithPath(p, path, count), true)
                    }
                  }
                  case _ => (MovementStateWithPath(movement, path, count), true)
                }

                //println(result)

                result
              },
              agg => agg.path,
              None
            )
            .take(1)

          source
        })

        Source.combine(sources(0), sources(1), sources(2), sources(3))(Merge(_)).filter(path => !path.isEmpty && path.last.x == startPosition.x && path.last.y == startPosition.y)
        //sources(0)
      })
      .runWith(sink)

    val path = Await.result(future, Duration.Inf)

    //println(path)

    val missingPipe = (path.last, path.head) match {
      case (MoveNorth(_, _), MoveNorth(_, _)) => '|'
      case (MoveSouth(_, _), MoveSouth(_, _)) => '|'
      case (MoveEast(_, _), MoveEast(_, _)) => '-'
      case (MoveWest(_, _), MoveWest(_, _)) => '|'
      case (MoveWest(_, _), MoveNorth(_, _)) => 'L'
      case (MoveSouth(_, _), MoveEast(_, _)) => 'L'
      case (MoveEast(_, _), MoveNorth(_, _)) => 'J'
      case (MoveSouth(_, _), MoveWest(_, _)) => 'J'
      case (MoveWest(_, _), MoveSouth(_, _)) => 'F'
      case (MoveNorth(_, _), MoveEast(_, _)) => 'F'
      case (MoveEast(_, _), MoveSouth(_, _)) => '7'
      case (MoveNorth(_, _), MoveWest(_, _)) => '7'
    }

    case class RowAccum(isIn: Boolean, pipeCurrent: Char, pipeStart: Option[Char], lastPosition: Option[PositionTrait], countIn: Long)

    path.groupBy(_.y).toList.sortBy(_._1).map {
      case (y, pointsInSameY) => {
        //println(pointsInSameY)
        val rowCountIn = pointsInSameY.sortBy(_.x).foldLeft(RowAccum(false, 'X', None, None, 0))((accum, position) => {
          val RowAccum(isIn, lastPipe, pipeStart, lastPosition, count) = accum
          val pipe = getCharInPosition(position).map(pipe => if pipe == 'S' then missingPipe else pipe)

          /*if (isIn && pipeStart == None) {
            println(s"I Should be counting between $lastPipe and $pipe")
          }*/

          val newCount = if (isIn && pipeStart == None) then (count + position.x - lastPosition.map(p => p.x).getOrElse(1) - 1L) else count

          val result = pipe match {
            case Success('|') => RowAccum(!isIn, '|', None, Some(position), newCount)
            case Success('L') => RowAccum(isIn, 'L', Some('L'), Some(position), newCount)
            case Success('F') => RowAccum(isIn, 'F', Some('F'), Some(position), newCount)
            case Success('-') => RowAccum(isIn, '-', pipeStart, Some(position), newCount)
            case Success('J') if pipeStart == Some('F') => RowAccum(!isIn, 'J', None, Some(position), newCount)
            case Success('J') => RowAccum(isIn, 'J', None, Some(position), newCount)
            case Success('7') if pipeStart == Some('L') => RowAccum(!isIn, '7', None, Some(position), newCount)
            case Success('7') => RowAccum(isIn, '7', None, Some(position), newCount)
          }

          //println(result)

          result
        }).countIn

        //println(rowCountIn)

        rowCountIn
      }
    }.sum
  }
}
