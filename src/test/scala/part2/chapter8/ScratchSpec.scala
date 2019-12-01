package part2.chapter8

import org.scalatest._
import org.scalacheck.{Gen, Prop}

class ScratchSpec extends FlatSpec with Matchers {
  "Scratch" should "sum correctly" in {
    val intList = org.scalacheck.Gen.listOf(org.scalacheck.Gen.choose(1, 100))

    val prop = org.scalacheck.Prop.forAll(intList)(is => Scratch.sum(is) == Scratch.sum(is.reverse))

    prop.check
  }

  "Sum of one number" should "be equals collection lenght * the number" in {
    val intList = org.scalacheck.Gen.listOf(1)

    val prop = org.scalacheck.Prop.forAll(intList)(is => Scratch.sum(is) == (is.length * is.headOption.getOrElse(0)))
  }
}
