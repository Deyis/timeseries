package denys.timeseries.writer


class Writer[T](headerFormatter: (Seq[String]) => String, formatter: T => String)(write: String => Unit) {
  def writeHeader(headers: String*): Unit = write(headerFormatter(headers))
  def write(value: T): Unit = write(formatter(value))
}
