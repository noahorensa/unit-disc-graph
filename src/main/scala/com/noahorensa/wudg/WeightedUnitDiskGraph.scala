package com.noahorensa.wudg

import scala.language.implicitConversions
import helpers._

import scala.collection.mutable
import scala.util.Random


class WeightedUnitDiskGraph[T <: Point](override val V: Seq[T]) extends Graph[T] {

  implicit def pointFromIndex(index: Int): T = V.find(_.index == index).get

  implicit def indexFromPoint(point: T): Int = point.index

  override lazy val E: Seq[WeightedEdge[T]] = V.combinations(2)
    .filter(pair => pair(0).dist(pair(1)) <= 1)
    .map(pair => WeightedEdge(pair(0), pair(1)))
    .toSeq

  def neighbors(p: Int): Seq[T] = E.map(e => {
    if (e.s.index == p) Option(e.t)
    else if (e.t.index == p) Option(e.s)
    else Option.empty
  }).filter(_.nonEmpty).map(_.get)

  def edges(p: Int): Seq[WeightedEdge[T]] = neighbors(p).map(WeightedEdge(V(p), _))

  def subset(points: Seq[Int]): Seq[T] = points.map(V(_))

  def pathsBetween(s: Int, t: Int): Seq[Path] = {

    def findPathsBetween(s: Path, t:Int): Seq[Path] = {

      if (s.points.last == t) {
        Seq(s)
      } else {
        neighbors(s.points.last.index).filter(n => ! s.points.contains(n)).flatMap(n => {
          findPathsBetween(Path(s.points :+ n), t)
        })
      }
    }

    findPathsBetween(Path(Seq(s)), t)
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

  def sparsify(epsilon: Double): WeightedUnitDiskGraph[Cell] = SparsefiedGraph(V, epsilon)

  lazy val H: WeightedUnitDiskGraph[Point] = {
    val v = V

    new WeightedUnitDiskGraph[Point](v) {
      override lazy val E: Seq[WeightedEdge[Point]] = delaunay(v).filter(_.weight <= 1)
    }
  }

  lazy val T: DecompositionTree[Point] = {

    def separate[Z <: Point](g: Graph[Z]) = {

      val bucket1 = mutable.ListBuffer[Z]()
      val bucket2 = mutable.ListBuffer[Z]()

      g.V.foreach(v => {
        if (bucket1.isEmpty || bucket1.exists(vv => g.E.exists(e => (e.s == v && e.t == vv) || (e.s == vv && e.t == v)))) bucket1.append(v)
        else bucket2.append(v)
      })

      Seq(bucket1, bucket2)
    }

    def decompose(g: WeightedUnitDiskGraph[Point], externalSeparators: Seq[Path]): DecompositionTree[Point] = {
      if (g.V.length < 4) {
        new DecompositionTree[Point](g.V, Seq(), externalSeparators, Seq())
      }
      else {
        val vertices = g.V.map(_.index)

        val separator = Random.shuffle(vertices).combinations(2)
          .map(pair => g.shortestPath(pair(0), pair(1)))
          .filter(_.nonEmpty)
          .map(_.get)
          .find(path => {
            val possiblySeparatedGraph = g.induceSubgraph(g.verticesExcept(path.points))
            val bucket = separate(possiblySeparatedGraph)
            bucket(0).nonEmpty && bucket(1).nonEmpty
          })

        if (separator.nonEmpty) {
          new DecompositionTree[Point](
            g.V,
            Seq(separator.get),
            externalSeparators,
            separate(g.induceSubgraph(g.verticesExcept(separator.get.points)))
              .map(subset => decompose(WeightedUnitDiskGraph(subset), Seq(separator.get)))
          )
        }
        else {
          new DecompositionTree[Point](g.V, Seq(), externalSeparators, Seq())
        }
      }
    }

    decompose(H, Seq())
  }

  def sparseNeighborhood(r: Double): Seq[Seq[T]] = {
    val subsets = mutable.ListBuffer[mutable.ListBuffer[T]]()
    val remaining = mutable.Set[T]()
    V.foreach(v => remaining.add(v))

    while (remaining.nonEmpty) {
      val s = mutable.ListBuffer[T]()

      def recursivelyAdd(v: T, dist: Double): Unit = {
        if (remaining.contains(v) && dist <= r) {
          s.append(v)
          remaining.remove(v)

          neighbors(v).foreach(n => recursivelyAdd(n, dist + v.dist(n)))
        }
      }

      recursivelyAdd(remaining.head, 0)
      subsets.append(s)
    }

    subsets
  }
}

object WeightedUnitDiskGraph {
  def apply(vertices: Seq[Point]): WeightedUnitDiskGraph[Point] = new WeightedUnitDiskGraph(vertices)

  def apply(vertices: Int): WeightedUnitDiskGraph[Point] = new WeightedUnitDiskGraph(
    ( for (i <- 0 until vertices) yield Point(i, math.random * 3, math.random * 3) )
  )
}


case class Cell(override val index: Int, override val x: Double, override val y: Double, points: Seq[Point]) extends Point(index, x, y) {

  def contains(p: Point): Boolean = points.exists(_.index == p.index)

  override def toString: String = s"$index ($x, $y) { ${points.mkString(", ")} }"
}

case class SparsefiedGraph(v: Seq[Point], epsilon: Double) extends WeightedUnitDiskGraph[Cell](Seq()) {

  override val V: Seq[Cell] = {
    val minX = v.minBy(_.x).x
    val maxX = v.maxBy(_.x).x
    val minY = v.minBy(_.y).y
    val maxY = v.maxBy(_.y).y

    var index = 0
    def newIndex = { val x = index; index += 1; x }

    for (i <- Range.Double((minY / epsilon).toInt * epsilon, maxY, epsilon)) yield {
      for (j <- Range.Double((minX / epsilon).toInt * epsilon, maxX, epsilon)) yield {
        val p = v.filter(p =>
          p.x >= j && p.x <= j + epsilon &&
            p.y >= i && p.y <= i + epsilon
        )

        if (p.nonEmpty) Option(Cell(newIndex, j, i, p))
        else Option.empty
      }
    }
    }.flatten.filter(_.nonEmpty).map(_.get)

  override lazy val E: Seq[WeightedEdge[Cell]] = V.combinations(2)
    .map(pair =>
      if (pair(0).points.exists(p => pair(1).points.exists(_.dist(p) <= 1)))
        Option(WeightedEdge(pair(0), pair(1)))
      else
        Option.empty
    ).filter(_.nonEmpty).map(_.get).toSeq
}

case class DecompositionTree[T <: Point](
  vertices: Seq[T],
  internalSeparators: Seq[Path],
  externalSeparators: Seq[Path],
  children: Seq[DecompositionTree[T]]
) {

  lazy val portals = internalSeparators.map(path => {
    val port = mutable.ListBuffer[Point]()

    var last = path.points.head
    port.append(last)
    for (p <- path.points) {
      if (p.dist(last) > 1.0) {
        port.append(p)
        last = p
      }
    }
    port
  })
}
