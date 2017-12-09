package nl.hugo.redbook.ch9

import org.scalatest.{ Matchers, WordSpec }

class Test9_15 extends WordSpec with Matchers {
  import MyParserTypes._
  import JSON._

  def parser: Parser[JSON] = JSON.jsonParser(LocationParser)

  def runPass(json: String, value: JSON): Unit =
    LocationParser.run(parser)(json) match {
      case Right(v) => v should be(value)
      case Left(error) => fail(s"$json\n$error")
    }

  def runFail(json: String): Unit =
    LocationParser.run(parser)(json) shouldBe a[Left[ParseError, _]]

  "JSON" should {

    "parse JSON null" in {
      runPass("[null]", JArray(IndexedSeq(JNull)))
    }

    "parse JSON booleans" in {
      runPass("[true]", JArray(IndexedSeq(JBool(true))))
      runPass("[false]", JArray(IndexedSeq(JBool(false))))
    }

    "parse JSON strings" in {
      runPass("[\"\"]", JArray(IndexedSeq(JString(""))))
      runPass("[\"Hello, world!\"]", JArray(IndexedSeq(JString("Hello, world!"))))
      // If any work says: Left as an exercise to the reader, it must be read as: I couldn't do it either. => Ignore
      //runPass("""["Escaped\"\b\f\n\r\t\/\\"]""", JArray(IndexedSeq(JString("Escaped\"\b\f\n\r\t/\\"))))
      runPass("""["Unicode\u048c\u159d\u26ae\u37bf"]""", JArray(IndexedSeq(JString("Unicode\u048c\u159d\u26ae\u37bf"))))
    }

    "parse JSON numbers" in {
      runPass("[0]", JArray(IndexedSeq(JNumber(0))))
      runPass("[42]", JArray(IndexedSeq(JNumber(42))))
      runPass("[-13]", JArray(IndexedSeq(JNumber(-13))))
      runPass("[3.14]", JArray(IndexedSeq(JNumber(3.14))))
      runPass("[-0.123]", JArray(IndexedSeq(JNumber(-0.123))))
      runPass("[1E6]", JArray(IndexedSeq(JNumber(1000000))))
      runPass("[0.7e-5]", JArray(IndexedSeq(JNumber(0.7e-5))))
      runPass("[123.45e+12]", JArray(IndexedSeq(JNumber(123.45e+12))))
    }

    "parse JSON arrays" in {
      runPass("[]", JArray(IndexedSeq.empty))
      runPass("[42]", JArray(IndexedSeq(JNumber(42))))
      runPass("""["Hello","world"]""", JArray(IndexedSeq(JString("Hello"), JString("world"))))
      runPass("""[13,true,null,"",[],{}]""", JArray(IndexedSeq(JNumber(13), JBool(true), JNull, JString(""), JArray(IndexedSeq.empty), JObject(Map.empty))))
    }

    "parse JSON objects" in {
      runPass("{}", JObject(Map.empty))
      runPass("""{"greeting":"Hello, world!"}""", JObject(Map("greeting" -> JString("Hello, world!"))))
      runPass("""{"answer":42}""", JObject(Map("answer" -> JNumber(42))))
      runPass("""{"present":true}""", JObject(Map("present" -> JBool(true))))
      runPass("""{"absent":null}""", JObject(Map("absent" -> JNull)))
      runPass("""{"list":[]}""", JObject(Map("list" -> JArray(IndexedSeq.empty))))
      runPass("""{"nested":{}}""", JObject(Map("nested" -> JObject(Map.empty))))
      runPass("""{"i":13,"b":false}""", JObject(Map("i" -> JNumber(13), "b" -> JBool(false))))
    }

    "ignore leading whitespace" in {
      runPass(" []", JArray(IndexedSeq.empty))
      runPass(" {}", JObject(Map.empty))
    }

    "ignore trailing whitespace" in {
      runPass("[] ", JArray(IndexedSeq.empty))
      runPass("{} ", JObject(Map.empty))
    }

    "ignore intermediate whitespaces" in {
      runPass("[ null]", JArray(IndexedSeq(JNull)))
      runPass("[null ]", JArray(IndexedSeq(JNull)))
      runPass("[null] ", JArray(IndexedSeq(JNull)))
      runPass("[null, null]", JArray(IndexedSeq(JNull, JNull)))
      runPass("[null ,null]", JArray(IndexedSeq(JNull, JNull)))
      runPass("""{"s":null}""", JObject(Map("s" -> JNull)))
      runPass("""{ "s":null}""", JObject(Map("s" -> JNull)))
      runPass("""{"s" :null}""", JObject(Map("s" -> JNull)))
      runPass("""{"s": null}""", JObject(Map("s" -> JNull)))
      runPass("""{"s":null }""", JObject(Map("s" -> JNull)))
      runPass("""{"s":null} """, JObject(Map("s" -> JNull)))
      runPass("""{"s":null,"t":null}""", JObject(Map("s" -> JNull, "t" -> JNull)))
      runPass("""{"s":null ,"t":null}""", JObject(Map("s" -> JNull, "t" -> JNull)))
      runPass("""{"s":null, "t":null}""", JObject(Map("s" -> JNull, "t" -> JNull)))
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
      val value = JObject(Map(
        "greeting" -> JString("Hello, world!"),
        "answer" -> JNumber(42),
        "nested" -> JObject(Map(
          "present" -> JBool(true),
          "absent" -> JNull,
          "list" -> JArray(IndexedSeq(JNumber(7), JBool(false), JNumber(3.14)))
        ))
      ))
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