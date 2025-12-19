import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.time.LocalDateTime
import scala.concurrent.duration.{FiniteDuration, DurationInt}
import java.time.Instant
import java.time.ZoneId

object TimeExercise extends IOApp {
  def tomorrow(): IO[FiniteDuration] = IO.realTime.map(_ + 1.day)
  def tomorrowDateTime(): IO[LocalDateTime] = tomorrow()
    .map(d => LocalDateTime.ofInstant(Instant.ofEpochMilli(d.toMillis), ZoneId.systemDefault()))

  override def run(args: List[String]): IO[ExitCode] = {
    tomorrowDateTime().flatTap(IO.println) *>
      tomorrow().map(_.toDays).flatTap(IO.println).as(ExitCode.Success)
  }
}
