package denys.timeseries.model


class WindowAggregate[T] private[model](private[model] val underlying: Vector[T])(keepIf: (T, T) => Boolean) {

  def value: Option[T] = underlying.headOption

  def addedToWindow(value: T): WindowAggregate[T] = {
    val lastIndex = underlying.lastIndexWhere(t => t == value || keepIf(t, value))
    val optimized = underlying.slice(0, lastIndex + 1)
    new WindowAggregate(optimized :+ value)(keepIf)
  }

  def removedFromWindow(value: T): WindowAggregate[T] = {
    val updated = underlying.headOption
      .filter(_ == value)
      .map(_ => underlying.drop(1))
      .getOrElse(underlying)
    new WindowAggregate(updated)(keepIf)
  }
}

object WindowAggregate {
  def apply[T](predicate: (T, T) => Boolean): WindowAggregate[T] = new WindowAggregate[T](Vector.empty[T])(predicate)
}