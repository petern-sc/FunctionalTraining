package com.rea.parser

import org.specs2.mutable.Specification
import Parsers._

class ParsersSpec extends Specification {
  "oneParser" in {
    oneParser.parse("one") must beRight(1)
  }

  "oneParser is not 1" in {
    oneParser.parse("two") must beLeft("could not find one")
  }

  "oneParser is 1" in {
    oneParser.parse("one and two") must beRight(1)
  }

  "twoParser" in {
    twoParser.parse("two") must beRight(2)
  }

  "twoParser is not 2" in {
    twoParser.parse("one") must beLeft("could not find two")
  }

  "oneOrTwoParser should parse one" in {
    oneOrTwoParser.parse("one") must beRight(1)
  }

  "oneOrTwoParser should parse two" in {
    oneOrTwoParser.parse("two") must beRight(2)
  }

  "number should parse two and give you string" in {
    number(oneOrTwoParser).parse("two") must beRight("You have 2 things")
  }

  "number should not be able to parse asd and returns error" in {
    number(oneOrTwoParser).parse("asd") must beLeft("could not find two")
  }

  "string must contain one and two" in {
    oneParser.and(twoParser).parse("one and two") must beRight((1,2))
  }

}
