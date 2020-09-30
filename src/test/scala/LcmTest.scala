import Main._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class LcmTest extends AnyFlatSpec {

  "lcm" must "calculate least Common Multiple" in {
    lcm(12, 15) shouldEqual Some(60)
    lcm(-15, 12) shouldEqual Some(60)
    lcm(-7, -11) shouldEqual Some(77)
    lcm(-933, 13) shouldEqual Some(12129)
    lcm(8, 12) shouldEqual Some(24)
    lcm(0, 12) shouldEqual None
    lcm(0, 0) shouldEqual None
  }
}
