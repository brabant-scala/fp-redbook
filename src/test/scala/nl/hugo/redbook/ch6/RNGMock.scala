package nl.hugo.redbook.ch6

case class RNGMock(value: Int, rng: RNG) extends RNG{
  override  def nextInt: (Int, RNG) = (value, rng)

}
