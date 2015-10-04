package lt.vpranckaitis.muller_preparata

import java.awt.{Color, Dimension, Graphics2D, _}

import lt.vpranckaitis.muller_preparata.Geometry.{Point, _}
import lt.vpranckaitis.muller_preparata.Process._

import scala.swing.BorderPanel.Position
import scala.swing.event._
import scala.swing.{Button, Panel, _}

object Main extends SimpleSwingApplication  {
  def top = new MainFrame() {
    title = "Muller-Preparata"
    size = new Dimension(500, 500)

    val Blacks = Seq.fill(50)(Color.BLACK)
    val Blues = Seq.fill(50)(Color.BLUE)
    val Colours = Seq(Color.RED, Color.GREEN, Color.BLUE,
                      Color.ORANGE, Color.PINK, Color.YELLOW,
                      Color.CYAN, Color.MAGENTA) ++ Blacks


    val steps: Seq[Seq[(Figure, Color)]] = {
      val data = mullerPreparata()

      import data._

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

        g.setColor(Color.GRAY)
        g.draw(Point(-width/2, 0).lineTo(Point(width/2, 0)))
        g.draw(Point(0, -height/2).lineTo(Point(0, height/2)))
        g.draw(Point(1, -0.1).lineTo(Point(1, 0.1)) * scale)
        g.draw(Point(-1, -0.1).lineTo(Point(-1, 0.1)) * scale)
        g.draw(Point(-0.1, 1).lineTo(Point(0.1, 1)) * scale)
        g.draw(Point(-0.1, -1).lineTo(Point(0.1, -1)) * scale)

        g.setStroke(new BasicStroke(3))
        for ((figure, color) <- steps(step)) {
          g.setColor(color)
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
