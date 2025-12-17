import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

import java.io._
import java.net.{HttpURLConnection, URL}

object ResourceSafetyExercise extends IOApp {

  def makeConnectionResource(targetURL: String) =
    Resource.make[IO, HttpURLConnection](createConnection(targetURL))(c => IO.blocking(c.disconnect()))

  def makeReaderResource(inputStream: InputStream) =
    Resource.make[IO, BufferedReader](IO.blocking(new BufferedReader(new InputStreamReader(inputStream)))) { is =>
      IO.println("releasing reader") *> IO.blocking(is.close)
    }

  def getResources(targetURL: String) = 
    for {
      connection <- makeConnectionResource(targetURL)
      reader <- makeReaderResource(connection.getInputStream)
    } yield (connection, reader)

  def createConnection(targetURL: String): IO[HttpURLConnection] =
    IO.blocking {
      val connection = new URL(targetURL).openConnection().asInstanceOf[HttpURLConnection]
      connection.setRequestMethod("GET")
      connection
    }

  def readOutput(reader: BufferedReader): IO[String] =
    IO.blocking {
      Iterator
        .continually(reader.readLine)
        .takeWhile(_ != null)
        .mkString("\n")
    }

  def httpGet(targetURL: String): IO[String] = {
    getResources(targetURL).use { 
      (_, reader) => readOutput(reader)
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    httpGet("http://www.google.com")
      .flatTap(IO.println)
      .as(ExitCode.Success)
  }
}
