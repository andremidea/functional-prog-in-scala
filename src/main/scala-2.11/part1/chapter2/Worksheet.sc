def fib(n: Int):Int = {

  @annotation.tailrec
  def go(prev: Int, cur: Int, acc: Int): Int = {
    if (acc == n) prev + cur
    else go(cur, prev + cur, acc + 1)
  }

  n match {
    case 0 => 0
    case 1 => 1
    case _ => go(0, 1, 2)
  }
}

val range = 0 until 10
range.map(fib _)



def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
  def f(index: Int): Boolean = {
    val a = as.lift(index - 1)
    val b = as.lift(index)

    (a, b) match{
      case (Some(x), Some(y)) => if(ordered(x, y)) f(index + 1) else false
      case _ => true
    }
  }

  as.length match {
    case 0 => true
    case _ => f(1)
  }
}


isSorted((1 until 10).toArray, (x: Int, y: Int) => x < y)
isSorted(Array(3, 3, 2, 5), (x: Int, y: Int) => x < y)


val lessThank = new ((Int, Int) => Boolean) {
  override def apply(v1: Int, v2: Int): Boolean = v1 < v2
}

def partial1[A,B,C](a: A, f: (A, B) => C): B => C =
  (b) => f(a, b)

val xuxu = (x: Int, y: Int) => x + y

val xuxu2 = partial1(1, xuxu)
xuxu2(2)


def curry[A,B,C](f: (A, B) => C): A => (B => C) =
  a => b => f(a, b)

val c1 = curry(xuxu)
val c2 = c1(1)
c2(4)

def uncurry[A,B,C](f: A => B => C): (A, B) => C =
  (a, b) => f(a).apply(b)

val u1 = uncurry(c1)
u1(1, 3)

def compose[A,B,C](f: B => C, g: A => B): A => C =
  (a) => f(g(a))

val s1 = (x: String) => x.toInt
val s2 = (x: Int) => (x * 2.0)

val composed = compose(s2, s1)
composed("2")

val s3 = s1 andThen s2
s3("2")

val s4 = s2.compose(s1)

s4("2")

