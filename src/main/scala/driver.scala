import com.noahorensa.wudg.{DecompositionTree, Graph, Point, WeightedUnitDiskGraph}
import driver.g
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.control.{Button, TextArea}
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Circle, Line}
import scalafx.scene.text.{Text, TextFlow}

object driver extends JFXApp {

  val SCALE = 120
  val g = WeightedUnitDiskGraph(23)
  val gHat = g.sparsify(0.5)

  val neighborhoods = for (i <- Range.Double(0, math.log(g.V.length) + 1, 1)) yield {
    val r = math.pow(2, i)
    (r, g.sparseNeighborhood(r))
  }

  stage = new JFXApp.PrimaryStage {
    width = SCALE * 16 + 100
    height = SCALE * 8 + 100

    scene = new Scene {
      fill = White
      content = drawGraph(g, 50, 50, Black, true) ++
        drawGraph(g.H, SCALE * 4 + 50, 50, Blue, true) ++
        drawGraph(gHat, SCALE * 8 + 50, 50, Red, false) ++
        neighborhoods.zipWithIndex.flatMap(nn =>
          nn._1._2.flatMap(n =>
            drawGraph(g.induceSubgraph(n), nn._2 * SCALE * 4 + 50, SCALE * 4, Color.color(math.random, math.random, math.random), true)
          )
        )
    }
  }

  g.V.foreach(p => println(s"${alpha(p)} (${p.x}, ${p.y})"))
  g.E.foreach(e => println(s"(${alpha(e.s)}, ${alpha(e.t)}) = ${e.weight}"))

  gHat.V.foreach(println)
  gHat.E.foreach(println)

  neighborhoods.foreach(n =>
    println(s"r = ${n._1}: ${n._2.map(v => s"[${v.map(alpha).mkString(", ")}]").mkString(", ")}")
  )

  def alpha(p: Point) = ('a' + p.index).asInstanceOf[Char].toString

  def drawGraph[T <: Point](g: Graph[T], xOff: Int, yOff:Int, color: Color, alphabet: Boolean) = {
    g.V.flatMap(p => Seq(new Circle(){
      centerX = p.x * SCALE + xOff
      centerY = p.y * SCALE + yOff
      radius = 4
      fill = color
    }, new Text(if (alphabet) alpha(p) else p.index.toString) {
      layoutX = p.x * SCALE + xOff - 15
      layoutY = p.y * SCALE + yOff - 15
      fill = color
    })) ++ g.E.map(e=> new Line() {
      startX = e.s.x * SCALE + xOff
      startY = e.s.y * SCALE + yOff
      endX = e.t.x * SCALE + xOff
      endY = e.t.y * SCALE + yOff
      stroke = color
    })
  }

  def printDecompositionTree(t: DecompositionTree[Point], depth: Int): Unit = {
    println(s"${"\t"*depth} [${t.vertices.map(_.index).mkString(", ")}], internalSep = ${t.internalSeparators.mkString(", ")}, externalSep = ${t.externalSeparators.mkString(", ")}, portals = {${t.portals.flatten.map(_.index).mkString(", ")}}")
    t.children.foreach(c => printDecompositionTree(c, depth + 1))
  }

  printDecompositionTree(g.T, 0)
}
