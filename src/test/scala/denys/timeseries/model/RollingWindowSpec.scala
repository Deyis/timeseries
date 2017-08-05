package denys.timeseries.model

import denys.timeseries.utils.RollingWindowUtils
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class RollingWindowSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  def measurementGen(timeGen: Gen[Long]) = for {
    time <- timeGen
    price <- Gen.posNum[Double]
  } yield Measurement(time, RollingWindowUtils.round(price))

  val partitionedMeasurements =  for {
    windowLength <- Gen.posNum[Int]
    if windowLength > 0
    m1 <- Gen.nonEmptyListOf(measurementGen(Gen.choose(1, windowLength)))
    start = windowLength + windowLength + 1
    end = windowLength + windowLength + 1 + windowLength
    m2 <- Gen.nonEmptyListOf(measurementGen(Gen.choose(start, end)))
  } yield (windowLength, m1, m2)

  val sortedMeasurements = for {
    windowLength <- Gen.posNum[Int]
    measurements <- Gen.nonEmptyListOf(measurementGen(Gen.posNum[Long])).map(_.sortBy(_.timestamp))
  } yield (windowLength, measurements)


  "Rolling window" should "return empty stats if it haven't receive any measurements" in {
    val expectedStats = RollingWindow.Stats(
      lastMeasure = None,
      n = 0,
      sum = 0,
      min = None,
      max = None
    )
    RollingWindow(windowLength = 60).getStats shouldEqual expectedStats
  }

  it should "shift end to received measure timestamp" in forAll(measurementGen(Gen.posNum[Long])) { measurement =>
    RollingWindow(windowLength = 60).shiftTo(measurement).getStats.lastMeasure shouldEqual Some(measurement)
  }

  it should "not count measures with timestamp older then last measure in window + window length" in forAll(partitionedMeasurements) {
    case (windowLength, group1, group2) =>
      val expectedStats = RollingWindow.Stats(
        lastMeasure = group2.lastOption,
        n = group2.size,
        sum = RollingWindowUtils.round(group2.map(_.price).sum),
        min = Some(group2.map(_.price).min).map(RollingWindowUtils.round),
        max = Some(group2.map(_.price).max).map(RollingWindowUtils.round)
      )
      (group1 ++ group2).foldLeft(RollingWindow(windowLength))((window, measurement) => window.shiftTo(measurement))
        .getStats shouldEqual expectedStats
  }

  it should "compute the same results as naive algorithm" in forAll(sortedMeasurements) { case (windowLength, measurements) =>
    val target = measurements.foldLeft(RollingWindow(windowLength))((window, measurement) => window.shiftTo(measurement))
    val naive = measurements.foldLeft(NaiveRollingWindow(windowLength))((window, measurement) => window.shiftTo(measurement))
    target.getStats shouldEqual naive.getStats
  }

  class NaiveRollingWindow(windowLength: Int)(
    elems: List[Measurement],
    n: Int,
    sum: Double,
    min: Option[Double],
    max: Option[Double]
  ){

    def shiftTo(measurement: Measurement): NaiveRollingWindow = {
      val updated = elems.filter(el => Math.abs(el.timestamp - measurement.timestamp) <= windowLength) :+ measurement
      val prices = updated.map(m => RollingWindowUtils.round(m.price))
      val newN = updated.size
      val newSum = prices.sum
      val newMin = if(updated.isEmpty) None else Some(prices.min)
      val newMax = if(updated.isEmpty) None else Some(prices.max)
      new NaiveRollingWindow(windowLength)(updated, newN, newSum, newMin, newMax)
    }

    def getStats: RollingWindow.Stats = RollingWindow.Stats(elems.lastOption, n, RollingWindowUtils.round(sum), min, max)
  }

  object NaiveRollingWindow{
    def apply(windowLength: Int): NaiveRollingWindow =
      new NaiveRollingWindow(windowLength)(List.empty, 0, 0, None, None)

  }
}
