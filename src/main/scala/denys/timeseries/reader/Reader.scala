package denys.timeseries.reader

import java.io.FileNotFoundException

import scala.io.Source
import scala.util.Try


object Reader {
  def read[T](source: => Source)(parser: String => Option[T]): Iterator[T] = using(source) { file =>
    file.getLines().flatMap(s => parser(s))
  }

  private def using[T](resource: => Source)(f: Source => T): T =
    Try(f(resource))
      .map(result => {
        resource.close()
        result
      })
      .recover({case ex: FileNotFoundException => f(Source.fromString(""))})
      .get
}
