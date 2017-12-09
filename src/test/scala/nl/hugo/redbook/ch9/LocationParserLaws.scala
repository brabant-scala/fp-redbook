package nl.hugo.redbook.ch9

import org.scalatest.PropSpec
import org.scalatest.prop.Checkers

class LocationParserLaws extends PropSpec with Checkers {
  property("charParserLaw") {
    check(LocationParser.Laws.charParserLaw)
  }
  property("stringParserLaw") {
    check(LocationParser.Laws.stringParserLaw)
  }
  property("orLaw") {
    check(LocationParser.Laws.orLaw)
  }
  property("listOfNLaw") {
    check(LocationParser.Laws.listOfNLaw)
  }

  property("manyLaw") {
    check(LocationParser.Laws.manyLaw)
  }

  property("productLaw") {
    check(LocationParser.Laws.productLaw)
  }

  property("productAssociativeLaw") {
    check(LocationParser.Laws.productAssociativeLaw)
  }

  property("many1Law") {
    check(LocationParser.Laws.many1Law)
  }

  property("sliceLaw") {
    check(LocationParser.Laws.sliceLaw)
  }
}
