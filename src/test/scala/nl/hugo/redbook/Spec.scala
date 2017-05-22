package nl.hugo.redbook

import org.scalatest.{ Matchers, WordSpec }

class Spec extends WordSpec with Matchers {
  implicit val warn: Warn = Warn(alert)
}