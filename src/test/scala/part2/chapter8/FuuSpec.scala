package part2.chapter8

import org.scalatest._

class FuuSpec extends FlatSpec with Matchers {
  "Fuu" should "have a test!" in {
    assert(1 === 1)
  }
}
