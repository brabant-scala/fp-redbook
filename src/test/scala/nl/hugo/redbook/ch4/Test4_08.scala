package nl.hugo.redbook.ch4

import org.scalatest.{ Matchers, WordSpec }

class Test4_08 extends WordSpec with Matchers {

  "map2" should {
    "What would you change in order to report both errors?" in {
      // map2 should not stop evaluation after the first error occurs.
      // Instead it should mark the outcome as a failure, but still continue evaluating
      // the remaining items, and if those lead to errors, append those errors to the
      // final Left, e.g. with newlines between errors.
    }

    "What would you change map2 or the signature of mkPerson?" in {
      // Changing the signature of mkPerson will not help. The map2 would still stop at first failing flatMap.
      // So map2 is the only place that you can change. It should do something like a left-map for the
      // remainder of the array once an error occurs.
    }

    "Or could you create a new data type that captures this requirement better than Either does, with some additional structure?" in {
      // Either[List[+E], +A]
    }

    "How would orElse, traverse, and sequence behave differently for that data type?" in {
      // orElse should append the possible "else" error to the errors that were already captured.
      // traverse and sequence use the adapted map2, so they should not need to be adapted.
    }
  }
}
