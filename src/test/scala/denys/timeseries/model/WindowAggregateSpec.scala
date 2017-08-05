package denys.timeseries.model

import org.scalacheck._
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class WindowAggregateSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Window aggregate" should "return empty current if window is empty" in {
    WindowAggregate[Any]((_, _) => true).value shouldBe empty
  }

  it should "return first element of window aggregation results as current value" in forAll(Gen.nonEmptyListOf[Int](Gen.posNum[Int])) { l =>
    new WindowAggregate[Int](l.toVector)((_, _) => true).value shouldEqual l.headOption
  }

  it should "remove not relevant aggregation values" in forAll(Gen.nonEmptyListOf[Int](Gen.posNum[Int])) { l =>
    val value = new WindowAggregate[Int](l.sorted(Ordering[Int].reverse).toVector)(_ > _)
    value.addedToWindow(Int.MaxValue).value shouldEqual Some(Int.MaxValue)
  }

  it should "not remove equal aggregation values" in forAll(Gen.nonEmptyListOf[Int](Gen.const(10))) { l =>
    val value = new WindowAggregate[Int](l.toVector)(_ > _)
    value.addedToWindow(10).underlying.size shouldEqual l.size + 1
  }

  it should "not remove value from window aggregations if it's not current value" in forAll(Gen.nonEmptyListOf[Int](Gen.posNum[Int])) { l =>
    val value = new WindowAggregate[Int](l.toVector)(_ > _).removedFromWindow(-1)
    value.underlying.size shouldEqual l.size
    value.value shouldEqual l.headOption
  }

  it should "remove only 1 value from window aggregations" in forAll(Gen.nonEmptyListOf[Int](Gen.const(10))) { l =>
    val value = new WindowAggregate[Int](l.toVector)(_ > _)
    value.removedFromWindow(10).underlying.size shouldEqual l.size - 1
  }
}
