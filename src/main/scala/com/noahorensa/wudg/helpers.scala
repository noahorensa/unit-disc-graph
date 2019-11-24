package com.noahorensa.wudg

import com.noahorensa.wudg.types.Triangle

object helpers {

  def det3x3(m: Array[Array[Double]]): Double = {
    (m(0)(0) * ((m(1)(1) * m(2)(2)) - (m(2)(1) * m(1)(2)))) +
    (m(0)(1) * ((m(1)(0) * m(2)(2)) - (m(2)(0) * m(1)(2)))) +
    (m(0)(2) * ((m(1)(0) * m(2)(1)) - (m(2)(0) * m(1)(1))))
  }

  def onSegment(p: Point, q:Point, r:Point):Boolean = {
    q.x <= Math.max(p.x, r.x) && q.x >= Math.min(p.x, r.x) &&
      q.y <= Math.max(p.y, r.y) && q.y >= Math.min(p.y, r.y)
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

  def isInside(triangle: Triangle, p: Point): Boolean = {
    val q = Point(p.index, 1000, p.y)

    Seq((triangle(0), triangle(1)), (triangle(1), triangle(2)), (triangle(2), triangle(0)))
      .count(l => doIntersect(p, q, l._1, l._2)) == 1
  }

}
