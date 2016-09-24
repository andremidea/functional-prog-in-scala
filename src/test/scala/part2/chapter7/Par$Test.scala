package part2.chapter7

import java.util.concurrent.Executors

import org.scalatest.FunSuite
import org.scalatest.concurrent.TimeLimits
import org.scalatest.time.SpanSugar._

class Par$Test extends FunSuite with TimeLimits {

  test("it should deadlock") {
    val e = Executors.newFixedThreadPool(2) // if i put 1 here it will certainly deadlock
    val a = Par.lazyUnit(1)
    failAfter(100 millis) {
      assert(Par.equal(e)(a, Par.fork(a)))
    }
  }

  test("it should deadlock even with more threads") {
    val e = Executors.newFixedThreadPool(5) // if i put 3 here it will certainly deadlock
    val a = Par.fork(Par.fork(Par.fork(Par.lazyUnit(1))))

    assert(Par.equal(e)(a, Par.fork(a)))
  }

  test("it should execute in another thread") {
    val e = Executors.newFixedThreadPool(2)
    val a = Par.lazyUnit(Thread.currentThread().getName())
    val ct = Thread.currentThread().getName
    val at = a(e).get()

    assert(ct != at)
  }

  test("it should execute in the same thread") {
    val e = Executors.newFixedThreadPool(2)
    val a = Par.delay(Par.unit(Thread.currentThread().getName()))
    val ct = Thread.currentThread().getName
    val at = a(e).get()
    assert(ct == at)
  }
}
