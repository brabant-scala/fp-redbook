package nl.hugo.redbook.ch6

import org.scalatest._

class Test6_10 extends WordSpec with Matchers {
//  "State" should {
//    "be initalized using the unit function" in {
//      State.unit(123).run(1) should be((123, 1))
//    }
//
//    "map its output using the map function" in {
//      State.unit(123).map(_.toString).run(1) should be(("123", 1))
//    }
//
//    "combine the output of two states using the map2 function" in {
//      State.unit[Int, Int](13).map2(State.unit(17))(_ + _).run(7) should be((30, 7))
//    }
//
//    "should apply flatMap" in {
//      State.unit(123).flatMap((i: Int) => State.unit[Int, Int](i + 1)).run(11) should be((124, 11))
//    }
//
//    "sequence a List of State" in {
//      val ints = List[State[Int, Int]](State.unit(1), State.unit(2), State.unit(3))
//
//      State.sequence(ints).run(11) should be((List(1, 2, 3), 11))
//    }
//  }
}
