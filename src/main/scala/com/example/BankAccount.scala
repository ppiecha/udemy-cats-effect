package com.example

import cats.effect.IOApp
import cats.effect.ExitCode
import cats.effect.IO
import cats.data.ValidatedNec
import cats.data.Validated
import cats.data.NonEmptyChain

object Controller {

  case class Request(fromAccount: String, toAccount: String, amount: String)
  case class Response(status: Int, body: String)

  def postTransfer(request: Request): IO[Response] = ???

}

object BankAccount extends IOApp {
  import Controller._
  override def run(args: List[String]): IO[ExitCode] =
    val request = Request("11111", "22222", "1000")
    postTransfer(request)
      .flatTap(IO.println)
      .as(ExitCode.Success)
}

object Validations {
  type Valid[A] = ValidatedNec[String, A]
  def validateDouble(s: String): Valid[Double] = 
    Validated.fromOption(s.toDoubleOption, NonEmptyChain(s"${s} is not valid double"))
  def validateAccountNumber(accountNumber: String): Valid[String] = 
    Validated.condNec(accountNumber.forall(_.isLetterOrDigit), accountNumber, "Not allowed characters")
}


