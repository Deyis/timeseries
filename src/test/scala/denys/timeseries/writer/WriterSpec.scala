package denys.timeseries.writer

import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}


class WriterSpec extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {


  "Writer" should "write according to provided formats" in forAll(Gen.nonEmptyListOf[Int](Gen.posNum[Int])) { l =>
    val output = new StringBuilder
    val writer = new Writer[Int](_ => "", _.toString)(s => output.append(s))
    l.foreach(writer.write)
    output.toString() shouldEqual l.mkString
  }
}
