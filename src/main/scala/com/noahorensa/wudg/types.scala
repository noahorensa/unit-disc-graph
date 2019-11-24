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
}

case class Edge(s: Point, t:Point) {

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

abstract class Graph() {
  val V: Seq[Point]
  val E: Seq[Edge]
}

case class GeneralGraph(override val V: Seq[Point], override val E: Seq[Edge]) extends Graph

object types {
  type Triangle = Seq[Point]

  implicit def tupleToTriangle(points: Tuple3[Point, Point, Point]): Triangle = Seq(points._1, points._2, points._3)
}