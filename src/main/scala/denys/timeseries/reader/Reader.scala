package denys.timeseries.reader

import java.io.FileNotFoundException

import scala.io.Source
import scala.util.Try


object Reader {
  def read[T](source: => Source)(parser: String => Option[T]): Iterator[T] = using(source) { file =>
    // todo find out what to do with invalid lines, right now they are just cleaned out
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
