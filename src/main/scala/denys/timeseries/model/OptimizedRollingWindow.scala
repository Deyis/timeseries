package denys.timeseries.model

import denys.timeseries.utils.RollingWindowUtils

trait RollingWindow {
  def shiftTo(measurement: Measurement): RollingWindow
  def getStats: RollingWindow.Stats
}

object RollingWindow {
  case class Stats(lastMeasure: Option[Measurement], n: Long, sum: Double, min: Option[Double], max: Option[Double])
}

class OptimizedRollingWindow private(windowLength: Int)(
    elems: List[Measurement],
    n: Int,
    sum: Double,
    min: WindowAggregate[Double],
    max: WindowAggregate[Double]
) extends RollingWindow {

  def shiftTo(measurement: Measurement): OptimizedRollingWindow = {
    val normilizedMeasurement = measurement.copy(price = RollingWindowUtils.round(measurement.price))

    val (excluded, left) = elems.partition(el => Math.abs(el.timestamp - normilizedMeasurement.timestamp) > windowLength)
    val newN = n - excluded.size + 1
    val newSum = sum - excluded.map(_.price).sum + normilizedMeasurement.price
    val newMin = excluded.foldLeft(min)(_ removedFromWindow _.price).addedToWindow(normilizedMeasurement.price)
    val newMax = excluded.foldLeft(max)(_ removedFromWindow _.price).addedToWindow(normilizedMeasurement.price)
    new OptimizedRollingWindow(windowLength)(left :+ normilizedMeasurement, newN, RollingWindowUtils.round(newSum), newMin, newMax)
  }

  def getStats: RollingWindow.Stats = RollingWindow.Stats(elems.lastOption, n, sum, min.value, max.value)
}

object OptimizedRollingWindow {
  def apply(windowLength: Int): OptimizedRollingWindow =
    new OptimizedRollingWindow(windowLength)(List.empty, 0, 0, WindowAggregate(_ < _), WindowAggregate(_ > _))
}


