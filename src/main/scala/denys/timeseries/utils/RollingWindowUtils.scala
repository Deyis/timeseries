package denys.timeseries.utils


object RollingWindowUtils {

  def round(d: Double): Double = (d * 100000).round.toDouble / 100000
}
