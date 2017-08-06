package denys.timeseries.writer

import denys.timeseries.model.RollingWindow


object Formats {
  val ROLLING_WINDOW_STATS_FORMATTER: RollingWindow.Stats => String = stats => (
    for {
      last <- stats.lastMeasure
      min <- stats.min
      max <- stats.max
    } yield "%-10d %-8.5f %-2d %-8.5f %-8.5f %-8.5f"
      .format(last.timestamp, last.price, stats.n, stats.sum, min, max)
  ).getOrElse("")

  private val underscore = List.fill((10 + 8 + 2 + 8 + 8 + 8 + 5/*spaces*/) / 2)("- ").mkString
  val ROLLING_WINDOW_HEADER_FORMATTER: Seq[String] => String = headers => {
    "%-10s %-8s %-2s %-8s %-8s %-8s\n%s".format(headers :+ underscore:_*)
  }

  val ROLLING_WINDOW_HEADERS = Seq("T", "V", "N", "RS", "MinV", "MaxV")
}
