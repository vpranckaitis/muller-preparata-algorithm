package lt.vpranckaitis.muller_preparata

import Geometry._

import scala.io.Source

object Process {
  def read(filename: String) = {
    def parse(lines: Stream[String]) = {
      lines map { l =>
        val r = l split ' ' map { _.toDouble }
        new Point(r(0), r(1))
      }
    }

    val lines = Source.fromFile(filename).getLines().toStream
    val (n, m) = (lines.head split ' ' map { _.toInt }) match {
      case a: Array[Int] => (a(0), a(1))
    }

    val (pn, pm) = (lines.tail splitAt n) match {
      case (p1, p2) => (p1, p2 take m)
    }

    (parse(pn), parse(pm))
  }

  def transform(p1: Point, p2: Point): Point = {
    val a = p1.y - p2.y
    val b = p2.x - p1.x
    val c = a * p1.x + b * p1.y

    new Point(a / c, b / c)
  }

  def transform(l: Line): Point = transform(l.p1, l.p2)

  def convexHull(points: Vector[Point]) = {
    val sorted = points.sorted

    def buildSide(comp: (Double, Double) => Boolean) = {
      sorted.tail.foldLeft(sorted take 1) { (acc: Vector[Point], p: Point) =>
        def drop(acc1: Vector[Point]): Vector[Point] = {
          if (acc1.size == 1) {
            acc1
          } else {
            val s = acc1(acc1.size - 2)
            val r = acc1(acc1.size - 1)

            val v1 = r - s
            val v2 = p - s
            if (comp(v1 cross v2, 0)) {
              drop(acc1.dropRight(1))
            } else {
              acc1
            }
          }
        }

        drop(acc) :+ p
      }
    }

    val top = buildSide(_ >= _)
    val bottom = buildSide(_ <= _)

    bottom ++ top.tail.reverse.tail
  }

  def process = {
    val (pn, pm) = read("input.txt")

    (for ((p1, p2) <- pn zip (pn.tail :+ pn.head)) yield {
      transform(p1, p2)
    }) ++ (for ((p1, p2) <- pm zip (pm.tail :+ pm.head)) yield {
      transform(p1, p2)
    })

    val ch = convexHull(pn.toVector ++ pm.toVector)

    println(ch mkString)

    (for ((p1, p2) <- ch zip (ch.tail :+ ch.head)) yield {
      new Line(p1, p2)
    })
  }
}
