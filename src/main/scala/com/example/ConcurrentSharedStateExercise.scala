import cats._
import cats.implicits._
import cats.effect._
import cats.effect.implicits._

object ConcurrentSharedStateExercise extends IOApp {
  case class User(username: String, age: Int, friends: List[User])

  // use ref to hold the current oldest user
  def loopUser(user: User, ref: Ref[IO, User]): IO[User] =
    if user.friends.isEmpty then ref.get
    else
      user.friends
        .parTraverse { user =>
          ref.update(u => if user.age > u.age then user else u) *> loopUser(user, ref)
        }
        .map(_.maxBy(_.age))

  def findOldest(user: User): IO[User] = {
    Ref.of[IO, User](user).flatMap { ref =>
      loopUser(user, ref)
    }
  }
  override def run(args: List[String]): IO[ExitCode] = {
    val a = User("a", 60, Nil)
    val b = User("b", 35, Nil)
    val c = User("c", 45, Nil)
    val d = User("d", 50, List(a, b))
    val e = User("e", 55, List(c))
    val f = User("f", 15, List(d, e))

    findOldest(f).flatTap(IO.println).as(ExitCode.Success)
  }
}
