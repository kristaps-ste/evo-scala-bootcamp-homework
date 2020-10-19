import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._
import ErrorHandlingHomeWork._
class ErrorHandlingTests extends AnyFlatSpec {
  "Payment card " must "be valid with cardnumber divided" in {
    val card = PaymentCardValidator.validate("John Smith", "5456-4951-7445-2342", "05/2022", "453")
    card.isValid shouldEqual true
  }
  "Payment card " must "be valid with cardnumber not divided" in {
    val card = PaymentCardValidator.validate("John Smith", "5456495174452342", "5/2022", "453")
    card.isValid shouldEqual true
  }
  "Payment card " must "be invalid due to checksum error" in {
    val card = PaymentCardValidator.validate("John Smith", "5956495174452342", "5/2022", "453")
    card.isValid shouldEqual false
  }
}
