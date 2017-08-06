package denys.timeseries

import denys.timeseries.model.{Measurement, OptimizedRollingWindow, RollingWindow}
import denys.timeseries.reader.{Parser, Reader}
import denys.timeseries.writer.Writer
import denys.timeseries.writer.Formats._

import scala.io.Source


object EntryPoint {

  def run(writer: Writer[RollingWindow.Stats], window: RollingWindow, reader: Iterator[Measurement]): Unit = {
    writer.writeHeader(ROLLING_WINDOW_HEADERS:_*)
    reader.foldLeft(window)((window, el) => {
      val newWindow = window.shiftTo(el)
      writer.write(newWindow.getStats)
      newWindow
    })
  }

  def main(args: Array[String]) {
    // todo find out how to propagate errors to the caller
    // meanwhile it's fine here to return invalid file name(Reader will not find the file and will return empty iterator)
    val filename = args.headOption.getOrElse("invalid")
    val writer = new Writer[RollingWindow.Stats](ROLLING_WINDOW_HEADER_FORMATTER, ROLLING_WINDOW_STATS_FORMATTER)(println)
    run(writer, OptimizedRollingWindow(windowLength =  60), Reader.read(Source.fromFile(filename))(Parser.MEASUREMENT_PARSER))
  }
}

