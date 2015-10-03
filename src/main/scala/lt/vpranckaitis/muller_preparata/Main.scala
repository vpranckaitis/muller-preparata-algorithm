package lt.vpranckaitis.muller_preparata

import java.awt.{Dimension, Graphics2D, Shape, Color}
import java.awt.geom._
import Geometry._
import Process._
import Geometry.Point

import scala.swing.BorderPanel.Position
import scala.swing._
import scala.swing.event._

object Main extends SimpleSwingApplication  {
  def top = new MainFrame() {
    title = "SwingApp"

    val Blacks = Seq.fill(50)(Color.BLACK)
    val Colours = Seq(Color.RED, Color.GREEN, Color.BLUE,
                      Color.ORANGE, Color.PINK, Color.YELLOW,
                      Color.CYAN, Color.MAGENTA) ++ Blacks


    val steps: Seq[Seq[(Figure, Color)]] = {
      val (pn, pm) = Process.read("input.txt")
      val polygons: Seq[Line] = polygon(pn) ++ polygon(pm)
      val dualPoints: Seq[Point] = polygons map transform
      val dualPointsConvexHull = polygon(convexHull(dualPoints.toVector))
      val undualPoints = dualPointsConvexHull map transform
      val intersection = polygon(undualPoints)

      val step1 = polygons zip Blacks
      val step2 = (polygons zip Colours) ++ (dualPoints zip Colours)
      val step3 = step1 ++ (dualPoints zip Blacks) ++ (dualPointsConvexHull zip Blacks)
      val step4 = step1 ++ (dualPoints zip Blacks) ++ (dualPointsConvexHull zip Colours) ++ (undualPoints zip Colours)
      val step5 = step1 ++ (dualPoints zip Blacks) ++ (dualPointsConvexHull zip Blacks) ++ (undualPoints zip Blacks) ++
        (intersection zip Seq.fill(50)(Color.BLUE))
      Seq(step1, step2, step3, step4, step5)
    }

    var step = 0

    object canvas extends Panel {
      preferredSize = new Dimension(300, 150)

      border = Swing.EmptyBorder(5, 5, 5, 5)

      override def paint(g: Graphics2D): Unit = {
        val (pn, pm) = Process.read("input.txt")
        g.setColor(Color.BLACK)
        val scale = 100
        //g.scale(10, 10)
        g.translate(200, 200)

        for ((figure, color) <- steps(step)) {
          g.setColor(color)
          g.draw(figure * scale)
        }

      }
    }

    contents = new BorderPanel {
      layout(canvas) = Position.Center
      layout(new Button()) = Position.North

    }
  }
}
