import cats.effect._
import cats.effect.implicits._
import cats.implicits._
import scala.concurrent.duration.DurationInt

object DeferredExercise extends IOApp {
  class Producer[A](name: String, deferred: Deferred[IO, A], exec: IO[A]) {
    def run(): IO[Unit] = exec.flatMap(a => deferred.complete(a).void)
  }

  class Consumer[A](name: String, deferred: Deferred[IO, A], consume: A => IO[Unit]) {
    def run(): IO[Unit] = deferred.get.flatMap(consume)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    Deferred[IO, String].flatMap { d => 
      (new Producer("producer", d, IO.sleep(1.second) *> IO.pure("Hello ")).run(), new Consumer("consumer", d, s => IO.println(s + "world")).run()) 
      .parMapN((_, s) => s)
      .as(ExitCode.Success)
     }
    //IO.pure(ExitCode.Success)
  }
}