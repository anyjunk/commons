package uk.co.anyjunk
package commons
package test

import io.typechecked.numerology.ternary.TNat._
import org.scalatest.{Matchers, FunSpec}

class FStringTest extends FunSpec with Matchers {

  describe("FString should") {

    it("successfully create a string of a correct length") {
      FString.fromString[t15]("012345678910").isRight should ===(true)
    }

    it("optionally create a string of a correct length") {
      FString.optionally[t15]("012345678910").isDefined should ===(true)
    }

    it("reject a string which is too long") {
      FString.fromString[t15]("012345678910111213").isLeft should ===(true)
      FString.optionally[t15]("012345678910111214").isEmpty should ===(true)
    }

    it("allow leading / trailing whitespace characters to count towards the length") {
      FString.fromString[t11](" " * 11).isRight should ===(true)
    }

    it("successfully add strings together") {
      val result: FString[t19] =
        FString.fromString[t8]("012345").getOrElse(sys.error("")) ++ FString.fromString[t11](" 67890").getOrElse(sys.error(""))

      result.toString should ===("012345 67890")
    }

    it("successfully add name with space together") {
      val result: FString[t17] =
        FString.fromString[t10]("Joe").getOrElse(sys.error("")) ++
          FString.WhiteSpace ++
          FString.fromString[t6]("Blog").getOrElse(sys.error(""))

      result.toString should ===("Joe Blog")
    }

    it("truncateTo strings as appropriate") {
      FString.truncateTo[t20]("Garden House School Housing Association Company")
        .getOrElse(sys.error("")).toString should ===("Garden House School")
    }

  }
}
