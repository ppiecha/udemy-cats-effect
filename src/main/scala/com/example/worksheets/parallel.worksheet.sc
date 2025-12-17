import cats.effect._
import cats.syntax.all._

case class Person(name: String)

object PersonSerwice {
  def createPerson(name: String): IO[Person] = Person(name).pure[IO]
  def createAll(persons: List[Person]): IO[List[Person]] = persons.traverse(_.pure[IO])
}