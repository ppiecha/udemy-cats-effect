import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import scala.io.StdIn
object CubeCalculator extends IOApp {

  def putStr(s: String): IO[Unit] = IO(println(s))
  def getStr(): IO[String] = IO {
    StdIn.readLine()
  }

  val program = for {
    _    <- putStr("Type your name:")
    name <- getStr()
    _    <- putStr(name)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = program.foreverM

}
