package nl.hugo.redbook.ch9

import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

class MyParserLaws extends PropSpec with Checkers {
  property("charParserLaw") {
    check(MyParser.Laws.charParserLaw)
  }
  property("stringParserLaw") {
    check(MyParser.Laws.stringParserLaw)
  }
  property("orLaw") {
    check(MyParser.Laws.orLaw)
  }
  property("listOfNLaw") {
    check(MyParser.Laws.listOfNLaw)
  }

  property("manyLaw") {
    check(MyParser.Laws.manyLaw)
  }

  property("productLaw") {
    check(MyParser.Laws.productLaw)
  }

  property("productAssociativeLaw") {
    check(MyParser.Laws.productAssociativeLaw)
  }

  property("many1Law") {
    check(MyParser.Laws.many1Law)
  }

  property("sliceLaw") {
    check(MyParser.Laws.sliceLaw)
  }
}
