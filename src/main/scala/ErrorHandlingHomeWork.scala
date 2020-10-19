import cats.data.ValidatedNec
import cats.implicits.{catsSyntaxTuple4Semigroupal, catsSyntaxValidatedIdBinCompat0}
import scala.util.Try

object ErrorHandlingHomeWork extends App {

  case class PaymentCard(
      name: HoldersName,
      number: CardNumber,
      expiration: ExpirationDate,
      securityCode: SecurityCode
  )

  //some ADT`s for PaymentCard
  final case class HoldersName(value: String)
  final case class CardNumber(value: String) extends AnyVal
  final case class ExpirationDate(month: String, year: String)
  final case class SecurityCode(value: Int) extends AnyVal

  sealed trait ValidationError
  object ValidationError {
    final case object InvalidNameCharacters extends ValidationError
    final case object InvalidNameLength extends ValidationError
    final case object CardNumberNotNumeric extends ValidationError
    final case object CardNumberInvalidLength extends ValidationError
    final case object CardNumberInvalidChecksum extends ValidationError
    final case object ExpirationDateInvalidFormat extends ValidationError
    final case object SecurityCodeNotNumeric extends ValidationError
    final case object SecurityCodeInvalidLength extends ValidationError
  }

  object PaymentCardValidator {
    import ValidationError._
    type AllErrorsOr[A] = ValidatedNec[ValidationError, A]

    def validate(
        name: String,
        number: String,
        date: String,
        securityCode: String
    ): AllErrorsOr[PaymentCard] =
      (validateName(name), validateNumber(number), validateExpirationDate(date), validateSecurityCode(securityCode))
        .mapN(PaymentCard)

    private def validateName(name: String): AllErrorsOr[HoldersName] = {

      def validateCharacters: AllErrorsOr[String] =
        if (name.trim.matches("[A-z\\s]+")) name.validNec else InvalidNameCharacters.invalidNec

      def validateLength(name: String): AllErrorsOr[HoldersName] =
        if (name.size < 3 || name.size > 24) InvalidNameLength.invalidNec else HoldersName(name).validNec

      validateCharacters andThen validateLength
    }

    private def validateExpirationDate(date: String): AllErrorsOr[ExpirationDate] = {

      if (!date.matches("(?:0?[1-9]|1[0-2])/202[0-9]")) { ExpirationDateInvalidFormat.invalidNec }
      else {
        val dateInParts = date.split("/")

        ExpirationDate(dateInParts(0), dateInParts(1)).validNec
      }
    }
    private def validateSecurityCode(code: String): AllErrorsOr[SecurityCode] = {
      def validateChars: AllErrorsOr[String] = {
        if (IsNumber(code)) code.validNec else SecurityCodeNotNumeric.invalidNec
      }
      def validateLength(code: String) =
        if (code.size == 3 || code.size == 4) SecurityCode(code.toInt).validNec
        else SecurityCodeInvalidLength.invalidNec

      validateChars andThen validateLength
    }
    private def validateNumber(number: String): AllErrorsOr[CardNumber] = {

      def validateCharacters: AllErrorsOr[String] = {
        val replaced = number.trim.replace("-", "")
        if (IsNumber(replaced)) replaced.validNec else CardNumberNotNumeric.invalidNec
      }

      def validateLength(toValidate: String): AllErrorsOr[String] =
        if (toValidate.size == 16 || toValidate.size == 15) toValidate.validNec else CardNumberInvalidLength.invalidNec

      // Checking number validity with Luhn’s checksum algorithm
      def validateCheckSum(number: String): AllErrorsOr[CardNumber] = {
        if (!IsNumber(number)) CardNumberNotNumeric.invalidNec
        else {
          val digitsWithIndex = number.split("").toList.map(_.toInt).reverse.zipWithIndex

          val oddSum = digitsWithIndex
            .filter { case (_, index) => index % 2 == 1 }
            .map { case (digit, _) => digit * 2 }
            .map(d => if (d > 9) d - 9 else d)
            .sum
          val evenSum = digitsWithIndex
            .filter { case (_, index) => index % 2 == 0 }
            .map { case (digit, _) => digit } sum

          if ((oddSum + evenSum) % 10 == 0) CardNumber(number).validNec else CardNumberInvalidChecksum.invalidNec
        }
      }
      validateCharacters andThen validateLength andThen validateCheckSum
    }
    private def IsNumber(value: String): Boolean = if (value.matches("[0-9]+")) true else false
  }
}
