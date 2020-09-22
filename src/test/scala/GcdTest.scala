import Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class GcdTest extends AnyFlatSpec {
  "gcd" must "calculate greatest common divisor" in {
    gcd(12, 8) shouldEqual 4
    gcd(8, 12) shouldEqual 4
    gcd(-8, 12) shouldEqual 4
    gcd(-22, 6) shouldEqual 2
    gcd(3200, 55) shouldEqual 5
    gcd(3200, 3200) shouldEqual 3200
    gcd(0, 2) shouldEqual 2
    gcd(2, 0) shouldEqual 2
    gcd(33, 999) shouldEqual 3
    gcd(-5000, 2300) shouldEqual 100
  }
}
