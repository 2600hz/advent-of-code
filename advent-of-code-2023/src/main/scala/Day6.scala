import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Sink, Source}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Day6 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  case class Data(time: Long, distance: Long)

  /*
  ((time - hold) * hold) > distance ===> 0 > hold^2 - time*hold + distance, which is a*x^2 + b*x + c, which can be resolved as (-b +- sqrt(b^2 - 4ac))/2a ===> (time +- sqrt(time^2 - 4*distance))/2
   */
  def calculateRange(time: Long, distance: Long): (Long, Long) = {
    val sqrt = Math.sqrt(Math.pow(time, 2) - (4 * distance))
    val min = (time - sqrt) / 2
    val max = (time + sqrt) / 2

    // As the solution for the equation must not be zero, but higher than zero,
    // we approximate the min solution of the quadratic equation to the nearest integer above,
    // and max to the nearest integer below
    ((min + 1).floor.longValue, (max - 1).ceil.longValue)
  }

  def runPart1 = {
    val fs = FileSystems.getDefault

    val sink = Sink.fold[Long, Long](1)(_ * _)

    val future = FileIO.fromPath(fs.getPath("../day6/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .map(line => {
        line.split(":\\ ").toList match {
          case List(_, values) => {
            println(values)
            values.trim.split("\\ +").map(_.toLong).toList
          }
        }
      })
      .sliding(2)
      .flatMapConcat(values => {
        val Seq(times, distances) = values
        val data = times.zip(distances).map((time, distance) => Data(time, distance))

        Source(data)
      })
      .map(data => {
        val (min, max) = calculateRange(data.time, data.distance)
        val numberOfWays = max - min + 1
        numberOfWays
      })
      .runWith(sink)
    //.runWith(Sink.ignore)

    Await.result(future, Duration.Inf)
  }

  def runPart2 = {
    val fs = FileSystems.getDefault

    val sink = Sink.fold[Long, Long](1)(_ * _)

    val future = FileIO.fromPath(fs.getPath("../day6/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .map(line => {
        line.split(":\\ ").toList match {
          case List(_, values) => {
            println(values)
            List(values.trim.replaceAll("\\ +", "").toLong) // Wrapped in a list just for plain laziness, to use the same code as part 1
          }
        }
      })
      .sliding(2)
      .flatMapConcat(values => {
        val Seq(times, distances) = values
        val data = times.zip(distances).map((time, distance) => Data(time, distance))

        Source(data)
      })
      .map(data => {
        val (min, max) = calculateRange(data.time, data.distance)
        val numberOfWays = max - min + 1
        numberOfWays
      })
      .runWith(sink)
    //.runWith(Sink.ignore)

    Await.result(future, Duration.Inf)
  }
}
