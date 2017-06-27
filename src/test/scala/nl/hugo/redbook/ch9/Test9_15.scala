package nl.hugo.redbook.ch9

import org.scalatest.Inspectors._
import nl.hugo.redbook.Spec

class Test9_15 extends Spec {
  import LocationParser._
  import JSON._

  def parser = JSON.jsonParser(Impl)

  def runPass(json: String, value: JSON): Unit =
    Impl.run(parser)(json) match {
      case Right(v) => v should be(value)
      case Left(error) => fail(s"$json\n$error")
    }

  def runFail(json: String): Unit =
    Impl.run(parser)(json) shouldBe a[Left[ParseError, _]]

  "JSON" should {

    "parse JSON null" in {
      runPass("null", JNull)
    }

    "parse JSON booleans" in {
      runPass("true", JBool(true))
      runPass("false", JBool(false))
    }

    "parse JSON strings" in {
      runPass("\"\"", JString(""))
      runPass("\"Hello, world!\"", JString("Hello, world!"))
      runPass(""""Escaped\"\b\f\n\r\t\/\\"""", JString("Escaped\"\b\f\n\r\t/\\"))
      runPass(""""Unicode\u048c\u159d\u26ae\u37bf"""", JString("Unicode\u048c\u159d\u26ae\u37bf"))
    }

    "parse JSON numbers" in {
      runPass("0", JNumber(0))
      runPass("42", JNumber(42))
      runPass("-13", JNumber(-13))
      runPass("3.14", JNumber(3.14))
      runPass("-0.123", JNumber(-0.123))
      runPass("1E6", JNumber(1000000))
      runPass("0.7e-5", JNumber(0.7e-5))
      runPass("123.45e+12", JNumber(123.45e+12))
    }

    "parse JSON arrays" in {
      runPass("[]", JArray())
      runPass("[42]", JArray(IndexedSeq(JNumber(42))))
      runPass("""["Hello","world"]""", JArray(IndexedSeq(JString("Hello"), JString("world"))))
      runPass("""[13,true,null,"",[],{}]""", JArray(IndexedSeq(JNumber(13), JBool(true), JNull, JString(""), JArray(), JObject())))
    }

    "parse JSON objects" in {
      runPass("{}", JObject())
      runPass("""{"greeting":"Hello, world!"}""", JObject("greeting" -> JString("Hello, world!")))
      runPass("""{"answer":42}""", JObject("answer" -> JNumber(42)))
      runPass("""{"present":true}""", JObject("present" -> JBool(true)))
      runPass("""{"absent":null}""", JObject("absent" -> JNull))
      runPass("""{"list":[]}""", JObject("list" -> JArray()))
      runPass("""{"nested":{}}""", JObject("nested" -> JObject()))
      runPass("""{"i":13,"b":false}""", JObject("i" -> JNumber(13), "b" -> JBool(false)))
    }

    "ignore whitespace" in {
      val json = s""" \t {
        "greeting" : "Hello, world!" ,
        "answer" : 42 ,

        \f

        "nested" : {
          "present" : true ,
          "absent" : null ,
          "list" : [ 7 , false , 3.14 ]
        }
      } \r """
      val value = JObject(
        "greeting" -> JString("Hello, world!"),
        "answer" -> JNumber(42),
        "nested" -> JObject(
          "present" -> JBool(true),
          "absent" -> JNull,
          "list" -> JArray(JNumber(7), JBool(false), JNumber(3.14))
        )
      )
      runPass(json, value)
    }

    "complain about trailing characters" in {
      runFail("""{ "greeting": "Hello, world!" } bla""")
    }

    "not parse invalid json" in {
      runFail("007")
      runFail("truly")
      runFail("\"Hello, world!")
      runFail(""""Es\cape"""")
      runFail(s""""Uni${'\\'}u123"""") // Scala complains about unicode anywhere (even in comments)
      runFail("""{ "answer": 42 """)
      runFail("""[ "Hello" "world!" ]""")
    }
  }
}