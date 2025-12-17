import cats.effect._
import cats.effect.unsafe.implicits._
import scala.io.StdIn

def putStr(s: String): IO[Unit] = IO(println(s))
def getStr(): IO[String]        = IO("simple text")

val r1 = putStr("first line")
r1.unsafeRunSync()

val r2 = putStr("Hello") >> putStr("World")
r2.unsafeRunSync()


val r3 = for {
  name <- getStr()
  _    <- putStr(name)
} yield ()

r3.unsafeRunSync()


