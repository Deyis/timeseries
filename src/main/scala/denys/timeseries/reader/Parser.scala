package denys.timeseries.reader

import denys.timeseries.model.Measurement

import scala.util.Try


object Parser {

  val MEASUREMENT_PARSER: String => Option[Measurement] = string =>
    string.split("\\s+") match {
      case Array(timestamp, price) => Try(Measurement(timestamp.toLong, price.toDouble)).toOption
      case _ => None
    }
}
