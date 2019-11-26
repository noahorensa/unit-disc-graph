package com.noahorensa.wudg


import scala.language.implicitConversions

case class Point(index: Int, x: Double, y: Double) {

  override def toString: String = s"$index ($x, $y)"

  def dist(other: Point): Double = {
    val diffX = x - other.x
    val diffY = y - other.y
    math.sqrt(diffX * diffX + diffY * diffY)
  }

  def slope(other: Point): Double = (y - other.y) / (x - other.x)

  override def equals(o: Any): Boolean =
    isInstanceOf[Point] && o.asInstanceOf[Point].index == index
}

abstract class Edge[T]() {
  val s: T
  val t: T
}

case class GeneralEdge[T](override val s: T, override val t: T) extends Edge[T]

case class WeightedEdge(override val s: Point, override val t: Point) extends Edge[Point] {

  def weight: Double = s.dist(t)

  override def toString: String = s"(${s.index}, ${t.index}) = $weight"
}

case class Path(points: Seq[Point]) {

  override def toString: String = s"[${points.map(_.index).mkString(", ")}] = $length"

  def length: Double = ( for (i <- 0 until points.length - 1) yield points(i).dist(points(i + 1)) ).sum

  def prepend(point: Point) = Path(point +: points)
}

case class VertexVisitList(g: WeightedUnitDiskGraph) {

  private val visited = g.V.map(_ => false).toArray

  def visit(p: Int): Unit = visited(p) = true

  def isVisited(p: Int): Boolean = visited(p)

  def unvisitedNeighbors(p: Int): Seq[Point] = g.neighbors(p).filter(pp => ! visited(pp.index))
}

abstract class Graph[T]() {
  val V: Seq[T]
  val E: Seq[Edge[T]]
}

case class GeneralGraph[T](override val V: Seq[T], override val E: Seq[Edge[T]]) extends Graph[T]

class Triangle(val points: Seq[Point]) {

  def contains(p: Point): Boolean = {
    import com.noahorensa.wudg.helpers.doIntersect

    val q = Point(p.index, 1000, p.y)

    Seq((points(0), points(1)), (points(1), points(2)), (points(2), points(0)))
      .count(l => doIntersect(p, q, l._1, l._2)) == 1
  }

  def onEdge(p: Point): Boolean = {
    import com.noahorensa.wudg.helpers.onSegment

    onSegment(points(0), p, points(1)) ||
    onSegment(points(1), p, points(2)) ||
    onSegment(points(2), p, points(0))
  }

  override def hashCode(): Int = points(0).index * 4 + points(1).index * 2 + points(2).index

  override def equals(o: Any): Boolean =
    o.isInstanceOf[Triangle] && o.asInstanceOf[Triangle].points.forall(points.contains(_))
}

object types {

  implicit def tupleToTriangle(points: Tuple3[Point, Point, Point]): Triangle =
    new Triangle(Seq(points._1, points._2, points._3))
}