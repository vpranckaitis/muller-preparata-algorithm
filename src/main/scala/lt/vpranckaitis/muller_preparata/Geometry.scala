package lt.vpranckaitis.muller_preparata

import java.awt.Shape
import java.awt.geom.{Ellipse2D, Line2D, Rectangle2D}

object Geometry {

  sealed trait Figure {
    def *(s: Double): Figure
  }

  case class Point(x: Double, y: Double) extends Ordered[Point] with Figure {
    def *(s: Double) = Point(x * s, y * s)
    def lineTo(p: Point) = Line(this, p)
    def cross(p: Point) = x * p.y - y * p.x
    def +(p: Point) = Point(x + p.x, y + p.y)
    def -(p: Point) = this + p * -1

    override def compare(that: Point): Int = {
      if (x == that.x) y compare that.y
      else x compare that.x
    }
  }

  case class Line(p1: Point, p2: Point) extends Figure {
    def *(s: Double) = Line(p1 * s, p2 * s)
  }

  def polygon(points: Seq[Point]) = for ((p1, p2) <- points zip (points.tail :+ points.head)) yield Line(p1, p2)

  implicit def figureToShape(f: Figure): Shape = f match {
    case p: Point => point2Shape(p)
    case l: Line => line2Shape(l)
  }

  private val PointSize = 4

  implicit def point2Shape(p: Point): Shape =
    new Ellipse2D.Double(p.x - PointSize / 2, p.y - PointSize / 2, PointSize, PointSize)

  implicit def line2Shape(l: Line): Shape = new Line2D.Double(l.p1.x, l.p1.y, l.p2.x, l.p2.y)
}
