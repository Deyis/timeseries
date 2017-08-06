package denys.timeseries

import denys.timeseries.model.{NaiveRollingWindow, OptimizedRollingWindow, RollingWindow}
import denys.timeseries.reader.{Parser, Reader}
import denys.timeseries.writer.Formats._
import denys.timeseries.writer.Writer
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source


class EntryPointSpec extends FlatSpec with Matchers {

  "EntryPoint" should "create the same out put with naive algorithm and optimized one" in {
    val output1 = new StringBuilder
    val writer1 = new Writer[RollingWindow.Stats](ROLLING_WINDOW_HEADER_FORMATTER, ROLLING_WINDOW_STATS_FORMATTER)(s => output1.append(s))

    val output2 = new StringBuilder
    val writer2 = new Writer[RollingWindow.Stats](ROLLING_WINDOW_HEADER_FORMATTER, ROLLING_WINDOW_STATS_FORMATTER)(s => output2.append(s))

    EntryPoint.run(writer1, OptimizedRollingWindow(windowLength =  60), Reader.read(Source.fromURL(getClass.getResource("/data_scala.txt")))(Parser.MEASUREMENT_PARSER))
    EntryPoint.run(writer2, NaiveRollingWindow(windowLength =  60), Reader.read(Source.fromURL(getClass.getResource("/data_scala.txt")))(Parser.MEASUREMENT_PARSER))

    output1.toString() shouldEqual output2.toString()
  }
}
