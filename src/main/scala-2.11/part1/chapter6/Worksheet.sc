// /chapter 6
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

type Rand[+A] = RNG => (A, RNG)

val rng = Simple(42)
rng.nextInt
rng.nextInt
Simple(rng.nextInt._1).nextInt
Int.MinValue
Int.MaxValue

RNG.nonNegativeInt(rng.nextInt._2)
RNG.double(rng.nextInt._2)
val (ints, rr) = RNG.ints(10)(rng)
rr.nextInt

val int: Rand[Int] = _.nextInt
def unit[A](a: A): Rand[A] =
  rng => (a, rng)

def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
  rng => {
    val (a, rng2) = s(rng)
    (f(a), rng2)
  }

def nonNegativeEven: Rand[Int] =
  map(RNG.nonNegativeInt)(i => i - i % 2)

def _double(rng: RNG): Rand[Double] =
  map(RNG.nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))

def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] = {
  rng => {
    val (v1, r1) = ra(rng)
    val (v2, r2) = rb(r1)
    val c = f(v1, v2)
    (c, r2)
  }
}

val map1: Rand[(Int, Double)] = map2(int, RNG.double)((_, _))

def both[A,B](ra: Rand[A], rb: Rand[B]): Rand[(A,B)] =
  map2(ra, rb)((_, _))

val randIntDouble: Rand[(Int, Double)] = both(int, RNG.double)

randIntDouble(rng)

def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
  rng => {
    fs.foldLeft((List.empty[A], rng))({
      case ((l, r), rand) => (rand(r)._1 :: l, rand(r)._2)
    })
  }


sequence(List.fill(10)(int))(rng)._1


def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
  rng => {
    val (v, r) = f(rng)
    g(v)(r) //(RNG) => (B, RNG)
  }

def nonNegativeLessThan(n: Int): Rand[Int] =
  flatMap(RNG.nonNegativeInt) { i =>
      val mod = i % n
      if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
  }

nonNegativeLessThan(1000)(rng)


def mapUsingFlat[A,B](s: Rand[A])(f: A => B): Rand[B] =
  flatMap(s)(x => unit(f(x)))

def map2UsingFlat[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A,B) => C): Rand[C] =
  flatMap(ra)(a => map(rb)(b => f(a, b)))


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

}

State.unit(1).map( x => x + 15).run(2)
State((s: Int) => (1 + s, s))
  .flatMap(x => State(s => (3, x)))
    .run(1)

State.modify((s: Int) => 10).run(1)