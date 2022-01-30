import java.io._
import scala.io.Source.fromURL
import scala.collection.mutable.Set
import scala.collection.mutable.Map
import scala.collection.mutable.Queue

object PageCrawler {
    val urlRegex = "href=\"/wiki/([\\w]+)\"".r

    def getLinksFromPage(pageContenst: String): Set[String] = {
        val links = Set[String]()
        for (link <- urlRegex.findAllIn(pageContenst)) {
            links += link.replace("href=\"/", "").replace("\"", "")
        }
        links
    }

    def getPageContent(url: String): String = {
        val source = fromURL(url)
        source.mkString
    }

    def crawl(entryPoint: String, maxPages: Int): Map[String, Set[String]] = {
        val pagesVisited = Map[String, Set[String]]()
        val pagesToVisit = Queue[String]()
        pagesToVisit += ""
        while (pagesVisited.size < maxPages && pagesToVisit.nonEmpty) {
            val currentPage = pagesToVisit.dequeue
            if (!pagesVisited.contains(currentPage)) {
                val pageContent = getPageContent(entryPoint + currentPage)
                val links = getLinksFromPage(pageContent)
                pagesVisited += (currentPage -> links)
                for (link <- links) {
                    pagesToVisit +=  link
                }
            }
        }
        pagesVisited
    }

    def saveToFile(pages: Map[String, Set[String]], fileName: String) = {
        val file = new File(fileName)
        val bw = new BufferedWriter(new FileWriter(file))
        for ((page, links) <- pages) {
            bw.write(page + ":")
            bw.write(links.toList.mkString(","))
            bw.newLine
        }
        bw.close
    }

    def main(args: Array[String]): Unit = {
        val url = args(0);
        val maxPages = args(1).toInt;
        val pagesVisited = crawl(url, maxPages)
        saveToFile(pagesVisited, "pages.txt")
        print(pagesVisited)
    }
}