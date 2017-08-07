package denys.timeseries.model


class WindowAggregation[T] private[model](private[model] val candidates: Vector[T])(keepIf: (T, T) => Boolean) {

  def value: Option[T] = candidates.headOption

  def addedToWindow(value: T): WindowAggregation[T] = {
    val lastIndex = candidates.lastIndexWhere(t => t == value || keepIf(t, value))
    val optimized = candidates.slice(0, lastIndex + 1)
    new WindowAggregation(optimized :+ value)(keepIf)
  }

  def removedFromWindow(value: T): WindowAggregation[T] = {
    val updated = candidates.headOption
      .filter(_ == value)
      .map(_ => candidates.drop(1))
      .getOrElse(candidates)
    new WindowAggregation(updated)(keepIf)
  }
}

object WindowAggregation {
  def apply[T](predicate: (T, T) => Boolean): WindowAggregation[T] = new WindowAggregation[T](Vector.empty[T])(predicate)
}