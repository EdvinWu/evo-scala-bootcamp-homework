package basics

object ClassesAndTraits {

  sealed trait Shape[T] extends Located with Bounded with Movable[T]

  sealed trait Located {
    def x: Double
    def y: Double
  }

  sealed trait Movable[T <: Shape[T]] {
    def move(dx: Double, dy: Double, dz: Double): Shape[T]
  }

  sealed trait Bounded {
    def minX: Double
    def maxX: Double
    def minY: Double
    def maxY: Double
  }

  sealed trait Shape2D[T] extends Shape[T] {
    def area: Double
  }

  sealed trait Shape3D[T] extends Shape[T] {
    def z: Double
    def minZ: Double
    def maxZ: Double
    def surfaceArea: Double
    def volume: Double
  }

  final case class Point(x: Double, y: Double, z: Double)
      extends Shape2D[Point]
      with Shape3D[Point] {
    override def minX: Double = x
    override def maxX: Double = x
    override def minY: Double = y
    override def maxY: Double = y

    override def move(dx: Double, dy: Double, dz: Double): Point =
      Point(x + dx, y + dy, z + dz)
    override def area: Double = 0
    override def minZ: Double = z
    override def maxZ: Double = z
    override def surfaceArea: Double = 0
    override def volume: Double = 0
  }

  final case class Square(x: Double, y: Double, side: Double)
      extends Shape2D[Square] {
    override def minX: Double = x
    override def maxX: Double = x + side
    override def minY: Double = y
    override def maxY: Double = y + side

    override def move(dx: Double, dy: Double, dz: Double): Square =
      Square(x + dx, y + dy, side)

    override def area: Double = side * side
  }

  final case class Sphere(centerX: Double,
                          centerY: Double,
                          centerZ: Double,
                          radius: Double)
      extends Shape3D[Sphere] {
    override def x: Double = centerX
    override def y: Double = centerY
    override def z: Double = centerZ

    override def minX: Double = centerX - radius
    override def maxX: Double = centerX + radius
    override def minY: Double = centerY - radius
    override def maxY: Double = centerY + radius
    override def minZ: Double = centerZ - radius
    override def maxZ: Double = centerZ + radius

    override def move(dx: Double, dy: Double, dz: Double): Sphere =
      Sphere(x + dx, y + dy, z + dz, radius)

    override def surfaceArea: Double = 4 * math.Pi * radius * radius

    override def volume: Double = (4 * math.Pi * math.pow(radius, 3)) / 3
  }
}
