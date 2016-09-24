package part2.chapter7

import java.util.concurrent._


object Par {
  type Par[A] = ExecutorService => Future[A]

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

  def delay[A](a: => Par[A]): Par[A] =
    es => a(es)

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

  def equal[A](e: ExecutorService)(a: Par[A], pa: Par[A]): Boolean = {
    a(e).get.equals(pa(e).get())
  }
}
