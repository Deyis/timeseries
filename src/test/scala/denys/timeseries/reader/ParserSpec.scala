package denys.timeseries.reader

import denys.timeseries.model.Measurement
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class ParserSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Parser" should "parse all correct strings" in forAll(Gen.posNum[Long], Gen.posNum[Double], Gen.oneOf(' ', '\t')) {
    case (timestamp, price, separator) =>
      Parser.MEASUREMENT_PARSER(s"$timestamp$separator$price") shouldEqual Some(Measurement(timestamp, price))
  }

  it should "return None for invalid lines" in forAll(Gen.alphaStr) { invalidMeasure =>
    Parser.MEASUREMENT_PARSER(invalidMeasure) shouldBe empty
  }
}
