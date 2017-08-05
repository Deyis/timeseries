package denys.timeseries.model

class RollingWindow private (
    elems: List[Measurement],
    n: Int,
    sum: Double,
    min: ???,
    max: ???
) {

  val windowLength = 60

  def shiftTo(measurement: Measurement): RollingWindow = {
    val (excluded, left) = elems.partition(el => Math.abs(el.timestamp - measurement.timestamp) > windowLength)
    val newN = n - excluded.size + 1
    val newSum = sum - excluded.map(_.price).sum + measurement.price
    val newMin = ???
    val newMax = ???
    new RollingWindow(left :+ measurement, newN, newSum, newMin, newMax)
  }

  def getStats: RollingWindow.Stats =
    RollingWindow.Stats(elems.lastOption, n, sum, min, max)
}

object RollingWindow {
  def apply(): RollingWindow = new RollingWindow(List.empty, 0, 0, ???, ???)

  case class Stats(lastMeasure: Option[Measurement], n: Long, sum: Double, min: Option[Double], max: Option[Double])
}


