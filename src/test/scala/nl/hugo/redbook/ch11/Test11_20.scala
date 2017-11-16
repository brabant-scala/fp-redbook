package nl.hugo.redbook.ch11

import nl.hugo.redbook.ch11.Monad._
import nl.hugo.redbook.ch6.State
import org.scalatest.{ Matchers, WordSpec }

class Test11_20 extends WordSpec with Matchers {
  "Reader[R,A] operations" in {
    case class Config(
      host: String,
      port: Int,
      path: String,
      query: String
    )
    val M = readerMonad[Config]

    val GOOGLE = Config("www.google.com", 80, "/search", "q")
    val urlReader = Reader((config: Config) => s"http://${config.host}:${config.port}/${config.path}?${config.query}")

    val expectedStr = ""
    M.replicateM(0, urlReader).run.apply(GOOGLE) should be(List())
    M.replicateM(1, urlReader).run.apply(GOOGLE) should be(List("http://www.google.com:80//search?q"))
    M.replicateM(2, urlReader).run.apply(GOOGLE) should be(List("http://www.google.com:80//search?q", "http://www.google.com:80//search?q"))

    M.sequence(
      List[Reader[Config, String]](
        M.unit("A"),
        urlReader,
        M.unit("C")
      )
    ).run(GOOGLE) should be(
        M.unit[List[String]](
          List("A", "http://www.google.com:80//search?q", "C")
        ).run(GOOGLE)
      )
  }
}