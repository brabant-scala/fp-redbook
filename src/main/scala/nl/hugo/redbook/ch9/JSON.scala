package nl.hugo.redbook.ch9

import language.higherKinds

trait JSON

object JSON {

  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  object JArray {
    def apply(elements: JSON*) = new JArray(elements.toIndexedSeq)
  }
  case class JObject(get: Map[String, JSON]) extends JSON
  object JObject {
    def apply(fields: (String, JSON)*) = new JObject(fields.toMap)
  }

  // Exercise 9.9
  def jsonParser[Parser[+_]](p: Parsers[Parser]): Parser[JSON] = ???
}