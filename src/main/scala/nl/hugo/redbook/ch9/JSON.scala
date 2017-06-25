package nl.hugo.redbook.ch9

import language.higherKinds

trait JSON

object JSON {

  // Section 9.4.1
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
  def jsonParser[Parser[+_]](p: Parsers[Parser]): Parser[JSON] =
    new JSONParser(p).parse

  private class JSONParser[Parser[+_]](p: Parsers[Parser]) {
    import p._

    private val jString: Parser[String] = scope("string")(token {
      val hex = regex("[A-Fa-f0-9]".r)
      val escaped: Parser[Char] =
        char('\\') -* oneOf(
          char('"'),
          char('\\'),
          char('/'),
          char('b').to('\b'),
          char('f').to('\f'),
          char('n').to('\n'),
          char('r').to('\r'),
          char('t').to('\t'),
          char('u') -* listOfN(4, hex).slice.map(h => Integer.parseInt(h, 16).toChar)
        )
      val chars = (regex("""[^"\\]""".r) | escaped).many
      chars.map(_.foldLeft("")(_ + _)).within("\"", "\"") // cannot use slice because of escaping
    })

    // TODO: try to write variant using Parsers
    private val jNumber: Parser[Double] = scope("number") {
      token(regex("-?(0|([1-9][0-9]*))(\\.[0-9]+)?([eE][-+]?[0-9]+)?".r).map(_.toDouble))
    }

    private def token[A](p: Parser[A]): Parser[A] = scope("token") {
      p *- ws
    }

    private def jField: Parser[(String, JSON)] = scope("field") {
      jString ** (t":" -* jValue)
    }

    private def jArray: Parser[JArray] = scope("array") {
      jValue.delimited(t",").within(t"[", t"]").map(l => JArray(l.toIndexedSeq))
    }

    private def jObject: Parser[JObject] = scope("object") {
      jField.delimited(t",").within(t"{", t"}").map(l => JObject(l.toMap))
    }

    private def jValue: Parser[JSON] = scope("value") {
      jString.map(s => JString(s)) |
        jNumber.map(JNumber(_)) |
        jArray |
        jObject |
        token("true").to(JBool(true)) |
        token("false").to(JBool(false)) |
        token("null").to(JNull)
    }

    def parse = root(ws -* jValue)

    private implicit class Token(context: StringContext) {
      def t[A](args: Any*): Parser[String] = token(context.s(args: _*))
    }
  }
}