package com.evolutiongaming.bootcamp.basics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import ControlStructuresHomework._
class ControlStructuresHomeworkTest extends AnyFlatSpec {
  "aa" should "bb" in {
    process("divide -6.76 9.5667") shouldEqual ("-6.76 divided by 9.5667 is -0.70662")
    process(
      " hj    j  87 k"
    ) shouldEqual ("Error: Failure in parsing numbers in parseCommand")
    process(
      "average -6 6 -6 -6 6 6 7.56 9.67 -99.5873125893456 "
    ) shouldEqual ("the average of -6 6 -6 -6 6 6 7.56 9.67 -99.58731 is -9.15081")
    process("sum   -6   7 ") shouldEqual ("the sum of -6 7 is 1")
    process("divide 1 0") shouldEqual ("Error: division by zero in calculate")
  }

}
