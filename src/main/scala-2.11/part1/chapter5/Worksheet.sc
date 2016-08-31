def maybeTwice(b: Boolean, i: () => Int): Int = {
  if (b) i() + i() else 0
}

maybeTwice(true, () => { println("aeee    "); 1+40; })

def maybeTwiceThunkLess(b: Boolean, i: => Int): Int = {
  if(b) i + i else 0
}
maybeTwiceThunkLess(true, {println("fuuu   "); 10})

def notTwice(b: Boolean, i: => Int): Int = {
  lazy val j = i
  if (b) j + j else 0
}

notTwice(true, {println("once    "); 10})

// --------------------------------------------------------------------------------------
// Stream
sealed trait Stream[+A] {
  def headOption[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  //5.1
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  //5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => Stream.cons(h(), t().take(n - 1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h,t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    this.foldRight(true)((a, b) => p(a) && b)

  def takeWhile2(p: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a, b) => if(p(a)) Stream.cons(a, b) else b)

  def headOption2(): Option[A] =
    this.foldRight(None: Option[A])((a, b) => Some(a))

  def map[B](f: A => B): Stream[B] =
    this.foldRight(Stream.empty[B])((a,b) => Stream.cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    this.foldRight(Stream.empty[A])((a,b) => if (f(a)) Stream.cons(a, b) else b)

  def append[B>:A](as: => Stream[B]): Stream[B] =
    this.foldRight(as)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    this.foldRight(Stream.empty[B])((a, b) => b append(f(a)))



  def mapU[B](f: A => B): Stream[B] = {
    Stream.unfold(this)({
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    })
  }

  def takeU(n: Int): Stream[A] = {
    Stream.unfold((this, n)) {
      case (s: Stream[A], x: Int) if x > 0 => s match {
        case Cons(h, t) => Some((h(), (t(), x -1)))
        case _ => None
      }
      case _ => None
    }
  }

  def takeWhileU(f: A => Boolean): Stream[A] = {
    Stream.unfold(this) {
      case Cons(h, t) if (f(h())) => Some(h(), t())
      case _ => None
    }
  }

  def zipWithU[B >: A, C](b: Stream[B])(f: (A,B) => C): Stream[C] = {
    Stream.unfold((this, b)) {
      case (Cons(h, t), Cons(j, k)) => Some((f(h(), j()), (t(), k())))
      case _ => None
    }
  }

  def zip[B >: A](b: Stream[B]): Stream[(A, B)] = {
    this.zipWithU(b)((_, _))
  }

  def startsWith[B >: A](s: Stream[B]): Boolean = {
    this.zip(s).forAll({case (x, y) => x == y})
  }

  def tails(): Stream[Stream[A]] = {
    Stream.unfold(this) {
      case Empty => None
      case s  => Some(s, s drop 1)
    } append Empty
  }

  def exists(f: (A) => Boolean): Boolean = {
    filter(f) != Empty
  }

  def hasSubsequence[B >: A](sub: Stream[B]): Boolean = {
    this.tails() exists(_ startsWith(sub))
  }

  def scanRight[Z](z: Z)(f: (Z, A) => Z): Stream[Z] = {
    foldRight((z, Stream(z)))((a, b) => {
      lazy val state = b
      val r = f(state._1, a)
      (r, Stream.cons(r, state._2))
    })._2
  }
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl

    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] = {
    Stream.cons(a, Stream.constant(a))
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n + 1))
  }

  def fibs(): Stream[Int] = {
    def go(prev: Int, cur: Int): Stream[Int] = {
      Stream.cons(cur, go(cur, cur + prev))
    }

    Stream.cons(0, go(0, 1))
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case Some((a: A, s: S)) => Stream.cons(a, unfold(s)(f))
      case _ => Empty
    }
  }

  def fromU(n: Int): Stream[Int] = {
    unfold(n)(z => Some(z, z + 1))
  }

  def constantU[A](a: A): Stream[A] = {
    unfold(a)(z => Some(a, a))
  }

}

val stream = Stream(1, 2, 3, 4)
stream.toList
stream.take(2).toList
stream.drop(2).toList
stream.takeWhile(_ < 4).toList
stream.foldRight(0)( _ + _)
stream.forAll(_ >= 3)
stream.forAll(_ >= 1)

stream.takeWhile2(_ < 4).toList
stream.headOption2()
Stream.empty.headOption2()
stream.map(_.toString).toList
stream.filter( _ % 2 == 0).toList
stream.append(Stream(8, 9, 10)).toList
stream.flatMap(Stream(_, 4, 5, 6)) toList

val ones: Stream[Int] = Stream.cons(1, ones)
ones.take(5).toList

Stream.constant("a").take(5).toList
Stream.from(2).take(10).toList
Stream.fibs().take(10).toList

Stream.unfold((0, 1)) { case (f0, f1) => Some((f0, (f1, f0 + f1)))}.take(10).toList

Stream.fromU(10).take(10).toList
Stream.constantU("a").take(10).toList

Stream.unfold(1)(z => Some(1, 1)).map(_ + 2).take(10).toList
ones.map(x => x).take(10).toList
stream.mapU(_ + 2).toList
stream.takeU(2).toList
stream.takeWhileU(_ < 3).toList
val twos: Stream[Int] = Stream.constantU(2)
stream.zip(twos).toList

stream.startsWith(twos)
stream.startsWith(stream)
stream.tails().map(_.toList).toList
stream.hasSubsequence(Stream(1, 2, 4))
stream.scanRight(0)(_ + _).toList