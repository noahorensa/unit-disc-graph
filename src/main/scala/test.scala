import com.noahorensa.wudg.{DecompositionTree, Point, WeightedUnitDiskGraph}

object test {
  def main(args: Array[String]): Unit = {
    val g = WeightedUnitDiskGraph(20)

    import WeightedUnitDiskGraph._

    g.V.foreach(println)

    val g_hat = g.sparsify(0.5)

    g_hat.V.foreach(c => {
      println(s"$c, SSSP = { ${
        g_hat.neighbors(c.index).map(cc => {
          val d = g_hat.dist(c.index, cc.index)
          if (d.nonEmpty) Option(s"${cc.index}: ${d.get}")
          else Option.empty
        }).filter(_.nonEmpty).map(_.get).mkString(", ")
      } }")
    })

    def printDecompositionTree(t: DecompositionTree[Point], depth: Int): Unit = {
//      println(s"V = {${t.vertices.map(_.index).mkString(", ")}}, internalSep = {${t.internalSeparators.mkString(", ")}}, externalSep = {${t.externalSeparators.mkString(", ")}}")
//      t.children.foreach(printDecompositionTree)

      println(s"${"\t"*depth} [${t.vertices.map(_.index).mkString(", ")}], internalSep = ${t.internalSeparators.mkString(", ")}, externalSep = ${t.externalSeparators.mkString(", ")}, portals = {${t.portals.flatten.map(_.index).mkString(", ")}}")
      t.children.foreach(c => printDecompositionTree(c, depth + 1))
    }

    printDecompositionTree(g.T, 0)
  }
}
