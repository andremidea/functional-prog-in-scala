import java.util.concurrent._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

//Exercise 7.1
//Par.map2 is a new higher-order function for combining the result
// of two parallel computations. What is its signature?
// Give the most general signature possible
// (don’t assume it works only for Int).

type Par[A] = ExecutorService => Future[A]

object Par {

  def unit[A](a: A): Par[A] = (es) => UnitFuture(a)

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def isDone: Boolean = true
    override def get(l: Long, timeout: TimeUnit): A = get
    override def isCancelled: Boolean = false
    override def cancel(b: Boolean): Boolean = false
  }

  def map2[A,B,C](a: Par[A], b: Par[B])(f: (A,B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get(), bf.get()))
    }

  def fork[A](a: => Par[A]): Par[A] =
    es => es.submit(new Callable[A] {
      def call(): A = a(es).get()
    })

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def asyncF[A,B](f: A => B): A => Par[B] =
    (a) => lazyUnit(f(a))

  def map[A,B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def sortPar(parList: Par[List[Int]]) = map(parList)(_.sorted)

  def sequence[A](ps: List[Par[A]]): Par[List[A]] = {
    ps.foldRight[Par[List[A]]](unit(List())){
      (p, list) => Par.map2(p, list)((a, list) => a :: list)
    }
  }

  def parMap[A,B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def max[A](seq: IndexedSeq[A])(f: (A, A) => A): Par[A] = {
    seq.length match {
      case 1 => unit(seq.last)
      case _ => {
        val at: (IndexedSeq[A], IndexedSeq[A])  = seq.splitAt(seq.size / 2)
        map2(max(at._1)(f), max(at._2)(f))(f)
      }
    }
  }

  def map3[A,B,C,D](a: Par[A], b: Par[B], c: Par[C])(f: (A,B,C) => D): Par[D] = {
    map2(map2(a, b)((x, y) => (z: C) => f(x, y, z)), c)(_ (_))
  }

  def choiceNOld[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    (ex) =>
      choices.lift(n(ex).get()).get.apply(ex)
  }

  def choice[A](c: Par[Boolean])(t: Par[A], f: Par[A]): Par[A] = {
    choiceN(map(c)(x => if(x) 1 else 0))(List(f, t))
  }

  def chooser[A, B](c: Par[A])(b: (A) => Par[B]): Par[B] = {
    (ex) =>
      b(c(ex).get).apply(ex)
  }

  def choiceN2[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    chooser(n)((x) => choices.lift(x).get)
  }

  def choiceMap[K,V](k: Par[K])(choices: Map[K,Par[V]]): Par[V] = {
    chooser(k)((x) => choices.get(x).get)
  }

  def join[A](c: Par[Par[A]]): Par[A] = {
    (ex) => c(ex).get.apply(ex)
  }

  def flatMap[A,B](ap: Par[A])(b: A => Par[B]): Par[B] = {
    join(map(ap)(b))
  }

  def choiceN[A](n: Par[Int])(choices: List[Par[A]]): Par[A] = {
    flatMap(n)((x) => choices.lift(x).get)
  }

  def flatMapToJoin[A,B](ap: Par[A])(b: A => Par[B]): Par[B] = {
    (ex) => b(ap(ex).get).apply(ex)
  }

  def joinUsingFlatMap[A](c: Par[Par[A]]): Par[A] = {
    flatMap(c)(x => x)
  }
}

val par = Par.unit(1)
Par.asyncF((x: Int) => x * 200)

val doSomething = (x: Int) => { Thread.sleep(1000); x * 1000}
val seqs = 1 until 10 toList
val randomSeq = 1 until 100 map( (_) => Random.nextInt())

val map: Par[List[Int]] = Par.parMap(seqs)(doSomething)

val executor = Executors.newCachedThreadPool()
val cexecutor = Executors.newFixedThreadPool(10)

map(executor).get()

val xuxu = Par.max(seqs.toIndexedSeq)((a: Int, b: Int) => if (a>b) a else b)
val xuxu2 = Par.max(randomSeq)((a: Int, b: Int) => { if (a>b) a else b})
xuxu(executor).get()
xuxu2(cexecutor).get()


/*

Free Theorem:

given map(y)(id) == y, it's a free theorem that:
   map(map(y)(g))(f) == map(y)(f compose g)

   map(map(y)(id))(f) == map(y)(f compose id)
   map(map(y)(id))(id) == map(y)(id)

   map(y)(id) == y then:

   map(id(y))(id) == map(y)(id)

   map(y)(id) == y
 */

val choicen = Par.choiceN(Par.unit(1))(List[Par[Int]](Par.unit(1), Par.unit(2), Par.unit(3)))
choicen(executor).get





