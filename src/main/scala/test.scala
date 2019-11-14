object test {
  def main(args: Array[String]): Unit = {

    val g = WeightedUnitDiskGraph(200)
    println("V")
    g.vertices.foreach(println)

//    println("\nE")
//    g.edges.foreach(println)

    println("\n\n")

    println(s"diameter = ${g.diameter}")
    println(s"radius = ${g.radius}")
  }
}
