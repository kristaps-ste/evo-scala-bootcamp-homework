import Homework2._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

class HomeWork2Test extends AnyFlatSpec {
  val testTriangle = Triangle(Point(5, 5), Point(-10, 30), Point(-5, -5))
  "Area for the triangle" should " be returned correctly" in {

    //are for triangles can referenced  in  https://www.mathopenref.com/coordtrianglearea.html
    testTriangle.area shouldEqual (200)
    testTriangle.area shouldNot be(2)
    val testTriangle2 = testTriangle.move(-1, -1, -1)
    testTriangle2.area shouldEqual 200
    testTriangle2.area shouldNot be(2)
  }

  "Triangular pyramid " should "be created from triangle" in {
    val pyramid = TriangularPyramid(testTriangle, Point(0, 0, -100))
    pyramid.boundsZ shouldEqual ((-100, 0))
    val pyramid2 = pyramid.move(0, 0, 1)
    pyramid2.boundsZ shouldEqual (-99, 1)
  }

  "Origin" should "be returned" in {
    val origin = Origin()
    origin.located.x shouldEqual origin.located.y
    origin.located.z shouldEqual (0.0)
  }

}
