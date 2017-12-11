package nl.hugo.redbook.ch12

import org.scalatest.{Matchers, WordSpec}

import scala.language.postfixOps

class Test12_06 extends WordSpec with Matchers {
  "An validationApplicative" should {
    val validationApplicative = Applicative.validationApplicative[String]

    "assign a value with unit" in {
      validationApplicative.unit(1) should be(Success(1))
    }

    "apply a value to a function" in {
      validationApplicative.apply(Success((v: Int) => v.toString))(Success(1)) should be(Success("1"))
    }

    val failedValue = Failure("value", Vector("1", "2"))
    val failedFunction = Failure("function", Vector("3", "4"))
    val errorMessages = (failedValue.head +: failedValue.tail) ++ (failedFunction.head +: failedValue.tail)

    "not apply a value to a failure" in {
      validationApplicative.apply(failedFunction)(Success(1)) should be(failedFunction)
    }

    "not apply a failure to a function" in {
      validationApplicative.apply(Success((v: Int) => v.toString))(failedValue) should be(failedValue)
    }

    "not apply a failure to a failure" in {
      val result = validationApplicative.apply(failedFunction)(failedValue).asInstanceOf[Failure[String]]

      // All the errormessages should be there, but ordering depends on implementatin dependent choices.
      (result.head +: result.tail).toSet should equal(errorMessages.toSet)
    }
  }
}