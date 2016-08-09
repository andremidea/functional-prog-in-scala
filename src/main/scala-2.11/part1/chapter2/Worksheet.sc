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

val x  = 1 until 10

val bla = for {
  a <- x.lift(5)
  b <- x.lift(8)
  c = a * b
} yield if (c > 10) c


//bla.map(x =>  x match {case (x: Int, y: Int) => x * y})

//bla match {
//  case Some(Tuple2(x, y)) => x * y
//  case _ => 0
//}




