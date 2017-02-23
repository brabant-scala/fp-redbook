package nl.hugo.redbook.ch4

import javafx.beans.binding.When

import nl.hugo.redbook.ch4.Ext.NonEmptyList
import org.scalatest.events.Summary
import org.scalatest.{ Matchers, WordSpec }

object Ext {
  final case class NonEmptyList[A](val head: A, val tail: List[A]) {
    def :::(a: A): NonEmptyList[A] = {
      new NonEmptyList(a, head :: tail)
    }
  }
  object NonEmptyList {
    def apply[A](head: A): NonEmptyList[A] = new NonEmptyList[A](head, Nil)
    def apply[A](head: A, t1: A): NonEmptyList[A] = new NonEmptyList[A](head, List(t1))
    def apply[A](head: A, t1: A, t2: A): NonEmptyList[A] = new NonEmptyList[A](head, List(t1, t2))
  }
  implicit class EitherOps[E, A](val a: Either[E, A]) extends AnyVal {
    def app2[B, C](b: Either[E, B])(f: (A, B) => C): Either[NonEmptyList[E], C] = {
      a match {
        case Left(err) => Left(NonEmptyList(err)).app2(b)(f)
        case Right(a) => Right[A](a).app2(b)(f)
      }
    }
  }
  implicit class AccumulatingEitherOps[E, A](val a: Either[NonEmptyList[E], A]) extends AnyVal {
    def app2[B, C](b: Either[E, B])(f: (A, B) => C): Either[NonEmptyList[E], C] = {
      (a, b) match {
        case (Left(errs), Left(err)) => Left(err ::: errs)
        case (Left(errs), Right(_)) => Left(errs)
        case (Right(_), Left(err)) => Left(NonEmptyList(err))
        case (Right(aa), Right(bb)) => Right(f(aa, bb))
      }
    }
  }
}

class Test4_08 extends WordSpec with Matchers {
  import Ext._

  "map2" should {

    case class Person(name: Name, age: Age)
    sealed class Name(val value: String)
    sealed class Age(val value: Int)

    def mkName(name: String): Either[String, Name] =
      if (name == "" || name == null) Left("Name is empty.")
      else Right(new Name(name))

    def mkAge(age: Int): Either[String, Age] =
      if (age < 0) Left("Age is out of range.")
      else Right(new Age(age))

    def mkPerson(name: String, age: Int): Either[NonEmptyList[String], Person] =
      mkName(name).app2(mkAge(age))(Person.apply)

    "What would you change in order to report both errors?" in {
      mkPerson("", -20) should equal(Left(NonEmptyList("Age is out of range.", "Name is empty.")))
    }

    "What would you change map2 or the signature of mkPerson?" in {
    }

    "Or could you create a new data type that captures this requirement better than Either does, with some additional sturcture?" in {
    }

    "How would orElse, traverse, and sequence behave differently for that data type?" in {
      // TODO
    }
  }
}