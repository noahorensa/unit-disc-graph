package com.noahorensa.wudg

import scala.language.implicitConversions

class WeightedUnitDiskGraph(override val V: Seq[Point]) extends Graph[Point] {

  implicit def pointFromIndex(index: Int): Point = V(index)

  implicit def indexFromPoint(point: Point): Int = point.index

  override lazy val E: Seq[WeightedEdge] = V.combinations(2)
    .filter(pair => pair(0).dist(pair(1)) <= 1)
    .map(pair => WeightedEdge(pair(0), pair(1)))
    .toArray

  def verticesExcept(p: Int): Seq[Point] = V.filter(_.index != p)

  def neighbors(p: Int): Seq[Point] = verticesExcept(p).filter(_.dist(V(p)) <= 1)

  def edges(p: Int): Seq[WeightedEdge] = neighbors(p).map(WeightedEdge(V(p), _))

  def subset(points: Seq[Int]): Seq[Point] = points.map(V(_))

  def pathsBetween(s: Int, t: Int): Seq[Path] = {
    val v = VertexVisitList(this)

    def findPathsBetween(s: Int, t:Int): Seq[Path] = {
      v.visit(s)

      if (s == t) {
        Seq(Path(Seq(V(t))))
      } else {
        v.unvisitedNeighbors(s)
          .flatMap(p => findPathsBetween(p, t).map(_.prepend(V(s))))
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
    val paths = V.combinations(2).map(pair => dist(pair(0), pair(1))).filter(_.nonEmpty).map(_.get)
    if (paths.nonEmpty) paths.max else Double.PositiveInfinity
  }

  def radius: Double = V
    .map(s => {
      val paths = verticesExcept(s).map(t => dist(s, t)).filter(_.nonEmpty).map(_.get)
      if (paths.nonEmpty) paths.max else Double.PositiveInfinity
    })
    .min
}

object WeightedUnitDiskGraph {
  def apply(vertices: Seq[Point]): WeightedUnitDiskGraph = new WeightedUnitDiskGraph(vertices.toArray)

  def apply(vertices: Int): WeightedUnitDiskGraph = new WeightedUnitDiskGraph(
    ( for (i <- 0 until vertices) yield Point(i, math.random * 10, math.random * 10) ).toArray
  )
}