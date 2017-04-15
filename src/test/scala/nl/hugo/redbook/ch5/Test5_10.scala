package nl.hugo.redbook.ch5

import nl.hugo.redbook.ch5.spec.FibsSpec

class Test5_10 extends FibsSpec {

  override def fibs = Stream.fibs

  fibsTest("fibs")

  "Stream" should {
    "generate fibonacci numbers" in {
      Stream.fibs2.take(7).toList should be(List(0, 1, 1, 2, 3, 5, 8))
    }
  }
}
