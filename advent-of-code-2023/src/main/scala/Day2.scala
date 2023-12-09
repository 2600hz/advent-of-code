import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Sink}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.postfixOps

case class GameData(Id: Int, cubeHandfuls: Array[Map[String, Int]])

object Day2 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  def parseGameData(line: String) = {
    val Array(game, data) = line.split(":").map(_.trim)
    val gameId = game.split(" ").last.toInt
    val cubeHandfuls = data.split(";").map(handful => {
        handful.trim.split(",").map(countPerColor => {
        val Array(count, color) = countPerColor.trim.split(" ")

        color -> count.toInt
        }).toMap
    })

    GameData(gameId, cubeHandfuls)
  }

  def runPart1 = {
    val availableCubes = Map(
      "red" -> 12,
      "green" -> 13,
      "blue" -> 14
    )

    def canPlayGameAux(cubes: Map[String, Int]) = (handful: Map[String, Int]) => {
      handful.forall((color, count) => {
        cubes.get(color) match {
          case Some(maxCount) => count <= maxCount
          case None => false
        }
      })
    }

    def canPlayGame(cubes: Map[String, Int]) = (gameData: GameData) => {
      gameData.cubeHandfuls.forall(canPlayGameAux(cubes))
    }

    val fs = FileSystems.getDefault

    val sink = Sink.fold[Int, Int](0)(_ + _)

    val future = FileIO.fromPath(fs.getPath("../day2/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .map(parseGameData)
      .filter(canPlayGame(availableCubes))
      .map(_.Id)
      .runWith(sink)
      //.runWith(Sink.ignore)

    Await.result(future, Duration.Inf)
  }

  def runPart2 = {
    val fs = FileSystems.getDefault

    val sink = Sink.fold[Int, Int](0)(_ + _)

    val future = FileIO.fromPath(fs.getPath("../day2/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .map(parseGameData)
      .map(gameData => {
        gameData.cubeHandfuls
          .fold(Map())((a, b) => {
            a ++ b.map { case (color, count) => color -> Math.max(count, a.getOrElse(color, 0)) }
          })
          .map((color, count) => count)
          .fold(1)((a, b) => a * (if b == 0 then 1 else b))
      })
      /*.map(sum => {
        println(sum)
        sum
      })*/
      .runWith(sink)
    //.runWith(Sink.ignore)

    Await.result(future, Duration.Inf)
  }
}
