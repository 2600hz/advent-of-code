import akka.actor.ActorSystem
import akka.stream.scaladsl.{FileIO, Framing, Sink, Source}
import akka.util.ByteString

import java.nio.file.FileSystems
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.language.postfixOps

object Day3 {
  implicit val system: ActorSystem = ActorSystem("QuickStart")

  def runPart1 = {
    val symbolRegex = "[^\\.\\d]".r
    val numberRegex = "\\d+".r

    val fs = FileSystems.getDefault

    val sink = Sink.fold[Int, Int](0)(_ + _)

    val extraLineSource = Source(List(""))

    val future = FileIO.fromPath(fs.getPath("../day3/input.txt"))
      .via(Framing.delimiter(ByteString("\n"), 256, true))
      .map(_.utf8String)
      .prepend(extraLineSource)
      .concat(extraLineSource)
      .sliding(3)
      .flatMapConcat(lines => {
        val symbolMatches = lines.flatMap(symbolRegex.findAllMatchIn(_).toList).toList
        val numberMatches = numberRegex.findAllMatchIn(lines(1)).toList

        val validNumberMatches = numberMatches
          .filter(numberMatch => {
            val fromIndex = numberMatch.start - 1
            val toIndex = numberMatch.end

            //println(s"$fromIndex, $toIndex")

            symbolMatches.exists(symbolMatch => symbolMatch.start >= fromIndex && symbolMatch.start <= toIndex)
          })
          .map(_.matched.toInt)

        Source(validNumberMatches)
      })
      /*.map(x => {
        println(x)
        x
      })*/
      .runWith(sink)

    Await.result(future, Duration.Inf)
  }
}
