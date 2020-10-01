package com.evolutiongaming.bootcamp.basics

import com.evolutiongaming.bootcamp.basics.ControlStructuresHomework.Command._

import scala.io.Source
import scala.util._
object ControlStructuresHomework {
  // Homework

  // Create a command line application that reads various "commands" from the
  // stdin, evaluates them, and writes output to stdout.

  // Commands are:

  //   divide 4 5
  // which should output "4 divided by 5 is 0.8"

  //   sum 5 5 6 8.5
  // which should output "the sum of 5 5 6 8.5 is 24.5"

  //   average 4 3 8.5 4
  // which should output "the average of 4 3 8.5 4 is 4.875"

  //   min 4 -3 -17
  // which should output "the minimum of 4 -3 -17 is -17"

  //   max 4 -3 -17
  // which should output "the maximum of 4 -3 -17 is 4"

  // In case of commands that cannot be parsed or calculations that cannot be performed,
  // output a single line starting with "Error: "

  sealed trait Command
  object Command {
    final case class Divide(dividend: Double, divisor: Double) extends Command
    final case class Sum(numbers: List[Double]) extends Command
    final case class Average(numbers: List[Double]) extends Command
    final case class Min(numbers: List[Double]) extends Command
    final case class Max(numbers: List[Double]) extends Command
  }

  final case class ErrorMessage(value: String)

  final case class Result(value: Double, operation: String, operands: List[Double]) {

    import java.text.DecimalFormat
    val df = new DecimalFormat("0.#####")
    def operandsStr: String = operands.map(df.format(_)).mkString(" ")
    def operandsStr(n: Int): String = df.format(operands(n))
    def valueStr: String = df.format(value)
  }

  def parseCommand(x: String): Either[ErrorMessage, Command] = {

    // Implementation hints:
    // You can use String#split, convert to List using .toList, then pattern match on:
    //   case x :: xs => ???

    // Consider how to handle extra whitespace gracefully (without errors).
    val splitted = x.trim().split("\\s+")
    val command: Option[String] = splitted.headOption.map(_.toLowerCase)

    val operands: List[Double] = Try(
      splitted.drop(1).map(_.toDouble).toList
    ) match {
      case Success(value) => value
      case Failure(error) => List()
    }

    import Command._
    (command, operands) match {
      case (None, _) =>
        generateLeftError("parseCommand", "None")

      case (Some(_), a) if a.isEmpty =>
        generateLeftError("parseCommand", "Failure in parsing numbers")

      case (Some("divide"), a) if a.size != 2 =>
        generateLeftError(
          "parseCommand",
          "Two operands exactly   must be provided for division"
        )
      case (Some("divide"), a)  => Right(Divide(a.head, a.last))
      case (Some("sum"), a)     => Right(Sum(a))
      case (Some("average"), a) => Right(Average(a))
      case (Some("min"), a)     => Right(Min(a))
      case (Some("max"), a)     => Right(Max(a))
      case (Some(a), _) =>
        generateLeftError("parseCommand", s"command parsed as $a")
    }
  }

  // should return an error (using `Left` channel) in case of division by zero and other
  // invalid operations
  def calculate(x: Command): Either[ErrorMessage, Result] = {
    x match {
      case Divide(_, b) if b == 0 =>
        generateLeftError("calculate", "division by zero")
      case Divide(a, b) => Right(Result(a / b, "div", List(a, b)))
      case Sum(numbers) => Right(Result(numbers.sum, "sum", numbers))
      case Average(numbers) if numbers.size == 0 =>
        generateLeftError("calculate", "0 operands ")
      case Average(numbers) =>
        Right(Result(numbers.sum / numbers.size, "avg", numbers))
      case Min(numbers) => Right(Result(numbers.min, "min", numbers))
      case Max(numbers) => Right(Result(numbers.max, "max", numbers))
      case _ =>
        generateLeftError("calculate", s" $x not implemented")
    }

  }

  def renderResult(x: Result): String = {

    x.operation match {
      case "div" if x.operands.size == 2 =>
        s"${x.operandsStr(0)} divided by ${x.operandsStr(1)} is ${x.valueStr}"
      case "sum" => f"the sum of ${x.operandsStr} is ${x.valueStr}"
      case "avg" => f"the average of ${x.operandsStr} is ${x.valueStr}"
      case "max" => f"the maximum of ${x.operandsStr} is ${x.valueStr}"
      case "min" => f"the minimum of ${x.operandsStr} is ${x.valueStr}"
      case _     => "Error: render error"
    }
  }
  def process(x: String): String = {
    import cats.implicits._
    val result = for {
      parsed <- parseCommand(x).leftMap(_.value)
      calculated <- calculate(parsed).leftMap(_.value)
    } yield renderResult(calculated)
    result.merge
  }

  // This `main` method reads lines from stdin, passes each to `process` and outputs the return value to stdout
  def main(args: Array[String]): Unit =
    Source.stdin.getLines() map process foreach println

  private def generateLeftError(
      methodName: String,
      text: String = ""
  ): Left[ErrorMessage, Nothing] =
    Left(ErrorMessage(s"Error: $text in $methodName"))
}
