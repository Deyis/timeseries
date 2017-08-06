package denys.timeseries.reader

import denys.timeseries.model.Measurement
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.Random


class ReaderSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  val measurementsGen = for {
    timestamp <- Gen.posNum[Long]
    price <- Gen.posNum[Double]
  } yield Measurement(timestamp, price)

  private def format(m: Measurement, sep: String): String = s"${m.timestamp}$sep${m.price}"

  "Reader" should "parse each line of source in original order" in forAll(Gen.nonEmptyListOf(measurementsGen), Gen.oneOf(" ", "\t")) {
    case (measurements, separator) =>
      val source = Source.fromString(measurements.map(format(_, separator)).mkString("\n"))
      Reader.read(source)(Parser.MEASUREMENT_PARSER).toList should contain theSameElementsInOrderAs measurements
  }

  it should "ignore invalid lines" in forAll(Gen.nonEmptyListOf(measurementsGen), Gen.oneOf(" ", "\t"), Gen.nonEmptyListOf(Gen.alphaStr)) {
    case (measurements, separator, invalidLines) =>
      val validLines = measurements.map(format(_, separator))
      val mixedLines = invalidLines.foldLeft(validLines)((res, line) => {
        val index = Random.nextInt(res.size)
        res.patch(index, List(line), 0)
      })

      val source = Source.fromString(mixedLines.mkString("\n"))
      Reader.read(source)(Parser.MEASUREMENT_PARSER).toList should contain theSameElementsInOrderAs measurements
  }

  it should "return empty iterator if file does not exist" in forAll(Gen.alphaStr) { file =>
    Reader.read(Source.fromFile(file))(Parser.MEASUREMENT_PARSER).toList shouldBe empty
  }
}
