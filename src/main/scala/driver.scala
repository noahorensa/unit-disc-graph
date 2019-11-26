import com.noahorensa.wudg.WeightedUnitDiskGraph
import scalafx.Includes._
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.paint.Color._
import scalafx.scene.shape.{Circle, Line}

object driver extends JFXApp {

  val SCALE = 80

  val g = WeightedUnitDiskGraph(150)
  private val vertices = g.V.map(p => new Circle(){
    centerX = p.x * SCALE + 50
    centerY = p.y * SCALE + 50
    radius = 4
    fill <== when(hover) choose Red otherwise Black
  })

  private val edges = g.E.map(e=> new Line() {
    startX = e.s.x * SCALE + 50
    startY = e.s.y * SCALE + 50
    endX = e.t.x * SCALE + 50
    endY = e.t.y * SCALE + 50
  })

  stage = new JFXApp.PrimaryStage {
    width = SCALE * 10 + 100
    height = SCALE * 10 + 100

    scene = new Scene {
      fill = White
      content = edges ++ vertices
    }
  }
}
