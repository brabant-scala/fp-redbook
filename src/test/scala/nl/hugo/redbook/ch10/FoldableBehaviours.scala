package nl.hugo.redbook.ch10

import nl.hugo.redbook.ch10.Helpers.{ appendL, appendR }
import nl.hugo.redbook.ch10.Monoid.stringMonoid
import org.scalatest.{ Matchers, WordSpec }

import scala.collection.generic.CanBuildFrom
import scala.language.higherKinds

trait FoldableBehaviours extends Matchers {
  this: WordSpec =>

  private def foldMapASequence[F[_]](fm: Foldable[F], l: F[Char], result: String) =
    s"foldMap a sequence to $result" in {
      fm.foldMap(l)(_.toString)(stringMonoid) shouldBe result
    }

  def aSeqFoldMapable[F[_] <: Seq[_]](fm: Foldable[F])(implicit cbf: CanBuildFrom[Nothing, Char, F[Char]]): Unit = {
    val l = Seq('a', 'b', 'c', 'd', 'e').to[F]
    val es = Seq.empty[Char].to[F]

    foldMapASequence(fm, l, "abcde")
    foldMapASequence(fm, es, "")
  }

  val tree = Branch(Leaf('a'), Branch(Branch(Leaf('b'), Leaf('c')), Branch(Leaf('d'), Leaf('e'))))
  val et = Leaf('L')

  def aTreeFoldMapable(fm: Foldable[Tree]): Unit = {
    foldMapASequence(fm, tree, "abcde")
    foldMapASequence(fm, et, "L")
  }

  def anOptionFoldMapable(fm: Foldable[Option]): Unit = {
    foldMapASequence(fm, None: Option[Char], "")
    foldMapASequence(fm, Some('A'), "A")
  }

  private def foldRightASequence[F[_]](fm: Foldable[F], l: F[Char], z: String, result: String) =
    s"foldRight a sequence to $result" in {
      fm.foldRight(l)(z)(appendR) shouldBe result
    }

  def aSeqFoldRightable[F[_] <: Seq[_]](fm: Foldable[F])(implicit cbf: CanBuildFrom[Nothing, Char, F[Char]]): Unit = {
    val l = Seq('a', 'b', 'c', 'd', 'e').to[F]
    val es = Seq.empty[Char].to[F]

    foldRightASequence(fm, l, "S", "Sedcba")
    foldRightASequence(fm, es, "S", "S")
  }

  def aTreeFoldRightable(fm: Foldable[Tree]): Unit = {
    foldRightASequence(fm, tree, "S", "Sedcba")
    foldRightASequence(fm, et, "S", "SL")
  }

  def anOptionFoldRightable(fm: Foldable[Option]): Unit = {
    foldRightASequence(fm, None: Option[Char], "S", "S")
    foldRightASequence(fm, Some('v'), "S", "Sv")
  }

  private def foldLeftASequence[F[_]](fm: Foldable[F], l: F[Char], z: String, result: String) =
    s"foldLeft a sequence to $result" in {
      fm.foldLeft(l)(z)(appendL) shouldBe result
    }

  def aSeqFoldLeftable[F[_] <: Seq[_]](fm: Foldable[F])(implicit cbf: CanBuildFrom[Nothing, Char, F[Char]]): Unit = {
    val l = Seq('a', 'b', 'c', 'd', 'e').to[F]
    val es = Seq.empty[Char].to[F]

    foldLeftASequence(fm, l, "S", "Sabcde")
    foldLeftASequence(fm, es, "S", "S")
  }

  def aTreeFoldLeftable(fm: Foldable[Tree]): Unit = {
    foldLeftASequence(fm, tree, "S", "Sabcde")
    foldLeftASequence(fm, et, "S", "SL")
  }

  def anOptionFoldLeftable(fm: Foldable[Option]): Unit = {
    foldLeftASequence(fm, None: Option[Char], "S", "S")
    foldLeftASequence(fm, Some('v'), "S", "Sv")
  }

  private def concatenateASequence[F[_]](fm: Foldable[F], l: F[String], result: String): Unit =
    s"concatenate a sequence to $result" in {
      fm.concatenate(l)(stringMonoid) shouldBe result
    }

  def aSeqConcatable[F[_] <: Seq[_]](fm: Foldable[F])(implicit cbf: CanBuildFrom[Nothing, String, F[String]]): Unit = {
    val l = Seq("a", "b", "c", "d", "e").to[F]
    val es = Seq.empty[String].to[F]

    concatenateASequence(fm, l, "abcde")
    concatenateASequence(fm, es, "")
  }

  def aTreeConcatable(fm: Foldable[Tree]): Unit = {
    val t = Branch(Leaf("a"), Branch(Branch(Leaf("b"), Leaf("c")), Branch(Leaf("d"), Leaf("e"))))
    val et = Leaf("L")

    concatenateASequence(fm, t, "abcde")
    concatenateASequence(fm, et, "L")
  }

  def anOptionConcatable(fm: Foldable[Option]): Unit = {
    concatenateASequence(fm, None: Option[String], "")
    concatenateASequence(fm, Some("v"), "v")
  }

  private def aListASequence[F[_]](fm: Foldable[F], l: F[String], result: List[String]): Unit = {
    s"toList a sequense to $result" in {
      fm.toList(l) shouldBe result
    }
  }

  val toListResult = List("a", "b", "c", "d", "e")

  def aSeqToList[F[_] <: Seq[_]](fm: Foldable[F])(implicit cbf: CanBuildFrom[Nothing, String, F[String]]): Unit = {
    val l = toListResult.to[F]
    val es = Seq.empty[String].to[F]

    aListASequence(fm, l, toListResult)
    aListASequence(fm, es, List.empty[String])
  }

  def aTreeToList(fm: Foldable[Tree]): Unit = {
    val tree = Branch(Leaf("a"), Branch(Branch(Leaf("b"), Leaf("c")), Branch(Leaf("d"), Leaf("e"))))
    val et = Leaf("L")

    aListASequence(fm, tree, toListResult)
    aListASequence(fm, et, List("L"))
  }

  def anOptionToList(fm: Foldable[Option]): Unit = {
    aListASequence(fm, None: Option[String], List.empty[String])
    aListASequence(fm, Some("A"), List("A"))
  }

}