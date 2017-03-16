package nl.hugo.redbook.ch6

object CNG {

  val zero = new CNG(0)

  // using implicit to make second argument list truly optional
  def apply(values: Int*)(implicit rng: RNG = zero): RNG =
    values.foldRight(rng) {
      (v, c) => new CNG(v, c)
    }
}

class CNG(value: Int, rng: Option[RNG] = None) extends RNG {

  def this(value: Int, rng: RNG) =
    this(value, Some(rng))

  override def nextInt: (Int, RNG) =
    (value, rng.getOrElse(this))

  override def toString: String =
    s"CNG($value,${rng.fold()(_.toString)})"
}