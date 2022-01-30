import scala.io.Source.fromFile
import scala.collection.mutable.Map

object PageRank {
    def readFromFile(fileName: String): Map[String, Set[String]] = {
        val lines = fromFile(fileName).getLines
        val result = Map[String, Set[String]]()
        for (line <- lines) {
            val parts = line.split(":")
            val page = parts(0)
            val links = parts(1).split(",")
            result += (page -> links.toSet)
        }
        result
    }

    def pageRank(links: Map[String, Set[String]], beta: Double, iteartions: Int): Map[String, Double] = {
        val pageRanks = links.foldLeft(Map[String, Double]()) {
            (map, link) => map += (link._1 -> 1.0 / links.size)
        }

        for (i <- 0 to iteartions) {
            for (node <- links.keys) {
                val sum = links.map(link => 
                    if (link._2.contains(node)) pageRanks(node) / links(node).size 
                    else 0.0)
                .sum

                pageRanks(node) = (1.0 - beta) + beta * sum
            }
        }

        pageRanks
    }

    def main(args: Array[String]): Unit = {
        val filename = args(0)
        val beta = args(1).toDouble
        val iterations = args(2).toInt

        print(pageRank(readFromFile(filename), beta, iterations))
    }
}