
case class Point(index: Int, x: Double, y: Double) {

  override def toString: String = s"$index ($x, $y)"

  def dist(other: Point): Double = {
    val diffX = x - other.x
    val diffY = y - other.y
    math.sqrt(diffX * diffX + diffY * diffY)
  }
}

case class WeightedEdge(s: Point, t:Point) {

  def weight: Double = s.dist(t)

  override def toString: String = s"(${s.index}, ${t.index}) = $weight"
}

case class Path(points: Seq[Point]) {

  override def toString: String = s"[${points.map(_.index).mkString(", ")}] = $length"

  def length: Double = ( for (i <- 0 until points.length - 1) yield points(i).dist(points(i + 1)) ).sum

  def prepend(point: Point) = Path(point +: points)
}

case class VertexVisitList(g: WeightedUnitDiskGraph) {

  private val visited = g.vertices.map(_ => false)

  def visit(p: Int): Unit = visited(p) = true

  def isVisited(p: Int): Boolean = visited(p)

  def unvisitedNeighbors(p: Int): Array[Point] = g.neighbors(p).filter(pp => ! visited(pp.index))
}

class WeightedUnitDiskGraph(val vertices: Array[Point]) {

  implicit def pointFromIndex(index: Int): Point = vertices(index)

  implicit def indexFromPoint(point: Point): Int = point.index

  lazy val edges: Array[WeightedEdge] = vertices.combinations(2)
    .filter(pair => pair(0).dist(pair(1)) <= 1)
    .map(pair => WeightedEdge(pair(0), pair(1)))
    .toArray

  def verticesExcept(p: Int): Array[Point] = vertices.filter(_.index != p)

  def neighbors(p: Int): Array[Point] = verticesExcept(p).filter(_.dist(vertices(p)) <= 1)

  def edges(p: Int): Array[WeightedEdge] = neighbors(p).map(WeightedEdge(vertices(p), _))

  def subset(points: Array[Int]): Array[Point] = points.map(vertices(_))

  def pathsBetween(s: Int, t: Int): Array[Path] = {
    val v = VertexVisitList(this)

    def findPathsBetween(s: Int, t:Int): Array[Path] = {
      v.visit(s)

      if (s == t) {
        Array[Path](Path(Seq(vertices(t))))
      } else {
        v.unvisitedNeighbors(s)
          .flatMap(p => findPathsBetween(p, t).map(_.prepend(vertices(s))))
      }
    }

    findPathsBetween(s, t)
  }

  def shortestPath(s: Int, t: Int): Option[Path] = {
    val paths = pathsBetween(s, t)
    if (paths.nonEmpty) Option(paths.minBy(_.length)) else Option.empty
  }

  def dist(s: Int, t: Int): Option[Double] = shortestPath(s, t).map(_.length)

  def diameter: Double = {
    val paths = vertices.combinations(2).map(pair => dist(pair(0), pair(1))).filter(_.nonEmpty).map(_.get)
    if (paths.nonEmpty) paths.max else Double.PositiveInfinity
  }

  def radius: Double = vertices
    .map(s => {
      val paths = verticesExcept(s).map(t => dist(s, t)).filter(_.nonEmpty).map(_.get)
      if (paths.nonEmpty) paths.max else Double.PositiveInfinity
    })
    .min
}

object WeightedUnitDiskGraph {
  def apply(vertices: Array[Point]): WeightedUnitDiskGraph = new WeightedUnitDiskGraph(vertices)

  def apply(vertices: Int): WeightedUnitDiskGraph = new WeightedUnitDiskGraph(
    ( for (i <- 0 until vertices) yield Point(i, math.random() * 10, math.random() * 10) ).toArray
  )
}