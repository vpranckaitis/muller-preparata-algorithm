package lt.vpranckaitis.muller_preparata

import java.awt.Color
import java.awt.Dimension
import java.awt.Graphics2D
import java.awt._
import java.awt.geom._
import Geometry._
import Process._
import Geometry.Point

import scala.swing.BorderPanel.Position
import scala.swing.Button
import scala.swing.Panel
import scala.swing._
import scala.swing.event._

object Main extends SimpleSwingApplication  {
  def top = new MainFrame() {
    title = "SwingApp"
    size = new Dimension(500, 500)

    val Blacks = Seq.fill(50)(Color.BLACK)
    val Blues = Seq.fill(50)(Color.BLUE)
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

      Seq(
        polygons zip Blacks,
        (polygons zip Colours),
        (polygons zip Colours) ++ (dualPoints zip Colours),
        (dualPoints zip Colours),
        (dualPointsConvexHull zip Blues) ++ (dualPoints zip Colours),
        dualPointsConvexHull zip Blues,
        dualPointsConvexHull zip Colours,
        (dualPointsConvexHull zip Colours) ++ (undualPoints zip Colours),
        (undualPoints zip Colours),
        (polygons zip Blacks) ++ (undualPoints zip Colours),
        (polygons zip Blacks) ++ (intersection zip Blues) ++ (undualPoints zip Colours),
        (polygons zip Blacks) ++ (intersection zip Blues)
      )
    }

    var step = 0

    object canvas extends Panel {
      preferredSize = new Dimension(500, 500)
      border = Swing.EmptyBorder(5, 5, 5, 5)

      override def paint(g: Graphics2D): Unit = {
        g.setColor(Color.WHITE)
        g.fill(g.getClip)

        val width = g.getClipBounds.width
        val height = g.getClipBounds.height
        val scale = 100
        //g.scale(10, 10)
        g.translate(width / 2, height / 2)

        for ((figure, color) <- steps(step)) {
          g.setColor(color)
          g.setStroke(new BasicStroke(3))
          g.draw(figure * scale)
        }

      }
    }

    val buttonNext = new Button { text = "Next" }
    val buttonPrevious = new Button { text = "Previous" }

    val box = new BoxPanel(Orientation.Horizontal) {
      contents ++= Seq(buttonPrevious, buttonNext)
    }

    contents = new BorderPanel {
      layout(canvas) = Position.Center
      layout(box) = Position.North

    }

    listenTo(buttonNext)
    listenTo(buttonPrevious)

    reactions += {
      case ButtonClicked(component) if component == buttonNext =>
        if (step < steps.size - 1) {
          step += 1
          this.repaint()
        }
      case ButtonClicked(component) if component == buttonPrevious =>
        if (step > 0) {
          step -= 1
          this.repaint()
        }
      case _ => println("some reaction")
    }
  }
}
