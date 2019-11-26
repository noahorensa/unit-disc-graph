package com.noahorensa.wudg

object helpers {

  def det3x3(m: Array[Array[Double]]): Double = {
    (m(0)(0) * ((m(1)(1) * m(2)(2)) - (m(2)(1) * m(1)(2)))) +
    (m(0)(1) * ((m(1)(0) * m(2)(2)) - (m(2)(0) * m(1)(2)))) +
    (m(0)(2) * ((m(1)(0) * m(2)(1)) - (m(2)(0) * m(1)(1))))
  }

  def onSegment(p: Point, q:Point, r:Point): Boolean = {
    q.x <= math.max(p.x, r.x) && q.x >= math.min(p.x, r.x) &&
      q.y <= math.max(p.y, r.y) && q.y >= math.min(p.y, r.y)
  }

  def orientation(p: Point, q: Point, r: Point): Int = {
    val x = (q.y - p.y) * (r.x - q.x) - (q.x - p.x) * (r.y - q.y)
    if (x == 0) return 0
    if (x > 0) 1
    else 2
  }

  def doIntersect(p1:Point, q1: Point, p2: Point, q2:Point): Boolean = {
    val o1 = orientation(p1, q1, p2);
    val o2 = orientation(p1, q1, q2);
    val o3 = orientation(p2, q2, p1);
    val o4 = orientation(p2, q2, q1);

    (o1 != o2 && o3 != o4) ||
    (o1 == 0 && onSegment(p1, p2, q1)) ||
    (o2 == 0 && onSegment(p1, q2, q1)) ||
    (o3 == 0 && onSegment(p2, p1, q2)) ||
    (o4 == 0 && onSegment(p2, q1, q2))
  }

  def findCircle(i: Point, j: Point, k: Point): (Point, Double) = {

    val x12 = i.x - j.x
    val x13 = i.x - k.x
    val y12 = i.y - j.y
    val y13 = i.y - k.y
    val y31 = k.y - i.y
    val y21 = j.y - i.y
    val x31 = k.x - i.x
    val x21 = j.x - i.x
    val sx13 = (math.pow(i.x, 2) - math.pow(k.x, 2)).toInt
    val sy13 = (math.pow(i.y, 2) - math.pow(k.y, 2)).toInt
    val sx21 = (math.pow(j.x, 2) - math.pow(i.x, 2)).toInt
    val sy21 = (math.pow(j.y, 2) - math.pow(i.y, 2)).toInt
    val f = (sx13 * x12 + sy13 * x12 + sx21 * x13 + sy21 * x13) / (2 * (y31 * x12 - y21 * x13))
    val g = (sx13 * y12 + sy13 * y12 + sx21 * y13 + sy21 * y13) / (2 * (x31 * y12 - x21 * y13))
    val c = -math.pow(i.x, 2).toInt - math.pow(i.y, 2).toInt - 2 * g * i.x - 2 * f * i.y

    val s = -g
    val t = -f
    val sqr_of_r = s * s + t * t - c
    val r = math.sqrt(sqr_of_r)

    (Point(0, s, t), r)
  }

  def delaunay(points: Seq[Point]): Seq[WeightedEdge] = {

    import org.locationtech.jts.geom.{Coordinate, GeometryFactory}
    import org.locationtech.jts.triangulate.DelaunayTriangulationBuilder
    import scala.collection.JavaConverters._

    val builder = new DelaunayTriangulationBuilder()

    builder.setSites(points.map(p => new Coordinate(p.x, p.y)).asJavaCollection)
    val edges = builder.getEdges(new GeometryFactory())

    for (i <- 0 until edges.getNumGeometries) yield {
      val geo = edges.getGeometryN(i)

      val c = geo.getCoordinates

      val s = points.minBy(_.dist(Point(-1, c(0).getX, c(0).getY)))
      val t = points.minBy(_.dist(Point(-1, c(1).getX, c(1).getY)))

      WeightedEdge(s, t)
    }
  }
}
