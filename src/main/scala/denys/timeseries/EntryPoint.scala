package denys.timeseries

import denys.timeseries.model.{Measurement, RollingWindow}
import denys.timeseries.reader.{Parser, Reader}
import denys.timeseries.writer.Writer
import denys.timeseries.writer.Formats._

import scala.io.Source


object EntryPoint {

  def main(args: Array[String]) {
    // todo find out how to propagate errors to the caller
    // meanwhile it's fine here to return invalid file name(Reader will not find the file and will return empty iterator)
    val filename = args.headOption.getOrElse("invalid")

    val writer = new Writer[RollingWindow.Stats](ROLLING_WINDOW_HEADER_FORMATTER, ROLLING_WINDOW_STATS_FORMATTER)(println)
    writer.writeHeader(ROLLING_WINDOW_HEADERS:_*)

    Reader.read(Source.fromFile(filename))(Parser.MEASUREMENT_PARSER).foldLeft(RollingWindow(windowLength =  60))((window, el) => {
      val newWindow = window.shiftTo(el)
      writer.write(newWindow.getStats)
      newWindow
    })
  }
}

