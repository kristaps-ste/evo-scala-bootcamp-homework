object Homework2 extends App {

  sealed trait Shape[A <: Movable[A]] extends Located with Movable[A]

  sealed trait Located {
    def located: Point
  }

  sealed trait Bounded2D {
    def boundsX: (Double, Double) //min / max coords stored in one tuple
    def boundsY: (Double, Double)

  }
  sealed trait Bounded3D extends Bounded2D {
    def boundsZ: (Double, Double)
  }
  sealed trait Movable[A <: Movable[A]] {
    // if move is intended in  2d space only , then it`s same as move in 3d with dz = 0
    def move(dx: Double, dy: Double, dz: Double = 0): Shape[A]
  }

  sealed trait Features2dObj {
    def area: Double
  }
  sealed trait Features3dObj {
    def surfaceArea: Double
    def volume: Double
  }
  // If it`s  a point in 2d plane ,  general 3d point with z coordinate = 0 can be used
  // Point is dimensionless, so no  2d or 3d features implemented in point
  final case class Point(x: Double, y: Double, z: Double = 0)
      extends Shape[Point]
      with Bounded3D {
    override def located: Point = Point(x, y, z)
    override def boundsX: (Double, Double) = (x, x)
    override def boundsY: (Double, Double) = (y, y)
    override def boundsZ: (Double, Double) = (z, z)
    def move(dx: Double, dy: Double, dz: Double = 0): Point =
      Point(x + dx, y + dy, z + dz)
  }

  // Origin is base point with (0,0,0) coords and not movable
  final case class Origin() extends Located with Bounded3D {
    override def located: Point = Point(0, 0)

    override def boundsZ: (Double, Double) = (0, 0)

    override def boundsX: (Double, Double) = (0, 0)

    override def boundsY: (Double, Double) = (0, 0)
  }

  final case class Circle(center: Point, radius: Double)
      extends Shape[Circle]
      with Bounded2D
      with Features2dObj {
    override def located: Point = center
    override def boundsX: (Double, Double) =
      (center.x - radius, center.x + radius)
    override def boundsY: (Double, Double) =
      (center.y - radius, center.y + radius)

    override def move(dx: Double, dy: Double, dz: Double = 0): Circle =
      Circle(Point(located.x + dx, located.y + dy, located.z + dz), radius)

    override def area: Double = Math.pow(radius, 2) * Math.PI
  }

  final case class Rectangle(
      topLeft: Point,
      width: Double,
      length: Double
  ) extends Shape[Rectangle]
      with Bounded2D
      with Features2dObj {
    override def located: Point = topLeft

    override def boundsX: (Double, Double) = (located.x, located.x + width)

    override def boundsY: (Double, Double) = (located.y - length, located.y)

    override def move(
        dx: Double,
        dy: Double,
        dz: Double = 0
    ): Rectangle =
      Rectangle(
        Point(located.x + dx, located.y + dy, located.z + dz),
        width,
        length
      )

    override def area: Double = width * length
  }

  final case class Square(topLeft: Point, diagonal: Double)
      extends Shape[Square]
      with Bounded2D
      with Features2dObj {
    override def located: Point = topLeft
    override def boundsX: (Double, Double) =
      (located.x, located.x + sideLength)

    override def boundsY: (Double, Double) =
      (located.y - sideLength, located.y)

    override def area: Double = diagonal * diagonal / 2

    override def move(dx: Double, dy: Double, dz: Double = 0): Square =
      Square(Point(located.x + dx, located.y + dy, located.z + dz), diagonal)

    def sideLength: Double = diagonal / Math.sqrt(2)
  }

  final case class Triangle(a: Point, b: Point, c: Point)
      extends Shape[Triangle]
      with Bounded2D
      with Features2dObj {
    override def located: Point = Point(a.x, a.y, a.z)
    val xCoords = List(a.x, b.x, c.x)
    val yCoords = List(a.y, b.y, c.y)
    val zCoords = List(a.z, b.z, c.z)
    override def boundsX: (Double, Double) = (xCoords.min, xCoords.max)
    override def boundsY: (Double, Double) = (yCoords.min, yCoords.max)
    override def move(dx: Double, dy: Double, dz: Double = 0): Triangle =
      Triangle(
        Point(a.x + dx, a.y + dy, a.z + dz),
        Point(b.x + dx, b.y + dy, b.z + dz),
        Point(c.x + dx, c.y + dy, c.z + dz)
      )
    override def area: Double =
      0.5 *
        (a.x * (b.y - c.y) + b.x * (c.y - a.y) + c.x * (a.y - b.y)).abs
  }
  final case class Ellipse(
      center: Point,
      xMax: Double,
      yMax: Double
  ) extends Shape[Ellipse]
      with Bounded2D
      with Features2dObj {

    override def located: Point = Point(center.x, center.y, center.z)

    override def boundsX: (Double, Double) = (center.x - xMax, center.x + xMax)

    override def boundsY: (Double, Double) = (center.y - yMax, center.y + yMax)

    override def area: Double = Math.PI * xMax * yMax

    override def move(dx: Double, dy: Double, dz: Double): Ellipse =
      Ellipse(Point(center.x + dx, center.y + dy, center.z + dz), xMax, yMax)
  }

  //3d shapes

  final case class Sphere(center: Point, radius: Double)
      extends Shape[Sphere]
      with Bounded3D
      with Features3dObj {

    override def located: Point = Point(center.x, center.y, center.z)

    override def boundsX: (Double, Double) =
      (center.x - radius, center.x + radius)

    override def boundsY: (Double, Double) =
      (center.y - radius, center.y + radius)
    override def boundsZ: (Double, Double) =
      (center.z - radius, center.z + radius)

    override def surfaceArea: Double = 4 * Math.pow(radius, 2) * Math.PI

    override def volume: Double = 4 / 3 * Math.pow(radius, 3) * Math.PI

    override def move(dx: Double, dy: Double, dz: Double): Sphere =
      Sphere(Point(center.x + dx, center.y + dy, center.z + dz), radius)
  }

  final case class Cube(base: Square, initialZ: Double, height: Double)
      extends Shape[Cube]
      with Bounded3D
      with Features3dObj {

    override def located: Point =
      Point(base.located.x, base.located.y, initialZ)

    override def boundsX: (Double, Double) = base.boundsX

    override def boundsY: (Double, Double) = base.boundsY

    override def boundsZ: (Double, Double) =
      (initialZ.min(initialZ + height), initialZ.max(initialZ + height))

    override def surfaceArea: Double = base.area * 6

    override def volume: Double = base.area * height

    override def move(dx: Double, dy: Double, dz: Double): Cube =
      Cube(
        Square(Point(base.located.x + dx, base.located.y + dy), base.diagonal),
        initialZ + dz,
        height
      )
  }

  final case class Cuboid(base: Rectangle, initialZ: Double, height: Double)
      extends Shape[Cuboid]
      with Bounded3D
      with Features3dObj {
    override def located: Point =
      Point(base.located.x, base.located.y, initialZ)

    override def boundsX: (Double, Double) = base.boundsX

    override def boundsY: (Double, Double) = base.boundsY
    override def boundsZ: (Double, Double) =
      (initialZ.min(initialZ + height), initialZ.max(initialZ + height))
    override def surfaceArea: Double =
      2 * (base.area + height * (base.width + base.length))
    override def volume: Double = base.area * height
    override def move(dx: Double, dy: Double, dz: Double): Cuboid =
      Cuboid(
        Rectangle(
          Point(base.located.x + dx, base.located.y + dy),
          base.width,
          base.length
        ),
        initialZ + dz,
        height
      )
  }
  //3d triangle can  be defined as base triangle with  a top point in 3d space
  final case class TriangularPyramid(base: Triangle, top: Point)
      extends Shape[TriangularPyramid]
      with Bounded3D
      with Features3dObj {
    override def located: Point = top
    override def boundsX: (Double, Double) = {
      val (xMin, xMax) = base.boundsX
      (xMin.min(top.x), xMax.max(top.x))
    }

    override def boundsY: (Double, Double) = {
      val (yMin, yMax) = base.boundsY
      (yMin.min(top.y), yMax.max(top.y))
    }
    override def boundsZ: (Double, Double) = {
      val zCoords = List(base.a.z, base.b.z, base.c.z, top.z)
      (zCoords.min, zCoords.max)
    }

    override def surfaceArea: Double = ???

    override def volume: Double = ???

    override def move(
        dx: Double,
        dy: Double,
        dz: Double
    ): TriangularPyramid =
      TriangularPyramid(
        Triangle(
          Point(base.a.x + dx, base.a.y + dy, base.a.z + dz),
          Point(base.b.x + dx, base.b.y + dy, base.a.z + dz),
          Point(base.c.x + dx, base.c.y + dy, base.c.z + dz)
        ),
        Point(top.x + dx, top.y + dy, top.z + dz)
      )

  }

  final case class Cylinder(base: Circle, height: Double)
      extends Shape[Cylinder]
      with Bounded3D
      with Features3dObj {
    override def located: Point = base.center
    override def boundsX: (Double, Double) = base.boundsX

    override def boundsY: (Double, Double) = base.boundsY
    override def boundsZ: (Double, Double) = {
      val zCoords = List(base.center.z, base.center.z + height)
      (zCoords.min, zCoords.max)
    }

    override def surfaceArea: Double =
      2 * (base.area + base.radius * height * Math.PI)

    override def volume: Double = base.area * height

    override def move(dx: Double, dy: Double, dz: Double): Cylinder =
      Cylinder(
        Circle(
          Point(located.x + dx, located.y + dy, located.z + dz),
          base.radius
        ),
        height
      )

  }
}
