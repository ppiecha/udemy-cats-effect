package com.example

import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import cats.data.NonEmptyChain
import cats.Traverse

class BankAccountTest extends org.scalatest.funsuite.AnyFunSuite {
  test("test positive validateDouble") {
    assert(Validations.validateDouble("1.32") === Valid(1.32))
  }
  test("test negative validateDouble") {
    assert(Validations.validateDouble("1d32") === Invalid(NonEmptyChain("1d32 is not valid double")))
  }
}
