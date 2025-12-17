import cats.effect._
import cats.implicits._
import scala.concurrent.duration.DurationInt

object MemoizationExercise extends IOApp {
  trait Currency
  case object Dollar extends Currency
  case object Euro   extends Currency

  case class Balance(amount: Double, currency: Currency)

  // pretend this is calling an API and takes some time
  def fetchDollarExchangeRate(currency: Currency): IO[Double] = {
    IO.sleep(2.seconds) *>
      IO.pure(currency match {
        case Dollar => 1.0
        case Euro   => 1.12
      })
  }

  val euroExchangeRate: IO[Double] = {
    IO.println("calling euroExchangeRate") *> fetchDollarExchangeRate(Euro)
  }

  def getBalancesInDollars(balances: List[Balance]): IO[List[Double]] = {
    euroExchangeRate.memoize.flatMap { exIO =>
      balances.traverse(b =>
        b.currency match
          case Dollar => IO.pure(b.amount)
          case Euro   => exIO.map(_ * b.amount)
      )
    }
  }

  override def run(args: List[String]): IO[ExitCode] = {
    // Modify both functions so they return an IO
    // Achieve the same behaviour:
    // - If all balances are dollars, you never fetch the exchange rate
    // - If more than one balance is euros, you only fetch the exchange rate once
    val balances = List(Balance(1.0, Dollar), Balance(2.0, Euro), Balance(3.0, Euro))
    getBalancesInDollars(balances).flatTap(IO.println).as(ExitCode.Success)
    //IO.pure(ExitCode.Success)
  }
}
