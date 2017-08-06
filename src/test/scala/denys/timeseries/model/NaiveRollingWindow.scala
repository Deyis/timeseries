package denys.timeseries.model

import denys.timeseries.utils.RollingWindowUtils

class NaiveRollingWindow(windowLength: Int)(
  elems: List[Measurement],
  n: Int,
  sum: Double,
  min: Option[Double],
  max: Option[Double]
) extends RollingWindow {

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

object NaiveRollingWindow {
  def apply(windowLength: Int): NaiveRollingWindow = new NaiveRollingWindow(windowLength)(List.empty, 0, 0, None, None)
}

