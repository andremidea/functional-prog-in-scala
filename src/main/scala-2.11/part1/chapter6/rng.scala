package part1.chapter6

trait RNG {
  def nextInt: (Int, RNG)
}

case class Simple(seed: Long) extends RNG {
  def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
    val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
    val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
    (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
  }

}
object RNG {
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val int: (Int, RNG) = rng.nextInt
    if (int._1 > Int.MinValue && int._1 <= Int.MaxValue)
      (Math.abs(int._1), int._2)
    else
      nonNegativeInt(int._2)
  }

  def rangeInt(rng: RNG, min: Int, max: Int): (Int, RNG) = {
    val int: (Int, RNG) = rng.nextInt
    if (int._1 > min && int._1 < max)
      int
    else
      rangeInt(int._2, min, max)
  }

  def double(rng: RNG): (Double, RNG) = {
    val int: (Int, RNG) = nonNegativeInt(rng)
    val d: Double = int._1.toDouble / Int.MaxValue.toDouble
    (d, int._2)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val int: (Int, RNG) = rng.nextInt
    val double1: (Double, RNG) = double(int._2)
    ((int._1, double1._1), double1._2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), r) = intDouble(rng)
    ((d, i), r)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    if (count == 0)
      (List(rng.nextInt._1), rng.nextInt._2)
    else {
      val (i, r) = rng.nextInt
      val (li, ri) = ints(count - 1)(r)
      (i :: li, ri)
    }
  }
}

case class State[S,+A](run: S => (A,S)) {

  def map[B](g: A => B): State[S, B] = {
    flatMap((v) => State.unit(g(v)))
  }


  def flatMap[B](g: A => State[S,B]): State[S, B] = {
    State { (s: S) =>
      val (a: A, st: S) = run(s)
      g(a).run(st)
    }
  }

  def map2[B,C](s2: State[S, B])(f: (A,B) => C): State[S, C] =
    flatMap(v => s2.map(s => f(v, s)))

}

case object State {
  def unit[S, A](v: A): State[S,A] = {
    State {(s: S) => (v, s)}
  }

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def sequence[S,A](states: List[State[S,A]]): State[S, List[A]] = {
    states.foldRight(unit[S, List[A]](List()))((st, list) => st.map2(list)(_ :: _))
  }

}
