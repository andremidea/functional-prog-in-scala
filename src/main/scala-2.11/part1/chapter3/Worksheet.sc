sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
 def sum(ints: List[Int]): Int = ints match {
  case Nil => 0
  case Cons(x, xs) => x + sum(xs)
 }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](x: List[A]): List[A] =
    x match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }


  def setHead[A](x: A, xs: List[A]) = xs match {
      case Nil => Cons(x, Nil)
      case Cons(head, tail) => Cons(x, tail)
    }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n == 0) return l
    l match {
      case Nil => Nil
      case Cons(head, tail) => drop(tail, n - 1)
    }
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Cons(head, tail) if f(head) =>dropWhile(tail, f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(head, Nil) => Nil
      case Cons(head, tail) => Cons(head, init(tail))
    }
  }
  def dropWhilePlus[A](l: List[A])(f: A => Boolean): List[A] = {
    dropWhile(l, f)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def length[A](as: List[A]): Int =
    foldRight(as, 0)((_, z) => z + 1)

  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }

  def reverse[A](as: List[A]): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) => foldLeft(xs, Cons(x, Nil))((x, y) => Cons(y, x))
  }

  def foldRightViaFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B =
    as match {
      case Nil => z
      case Cons(x, xs) => List.foldLeft(List.reverse(as), z)((b, a) => f(a, b))
    }

  def append[A](as: List[A], a: List[A]): List[A] =
    foldRight(as, a)(Cons(_, _))

  def concat[A](a1: List[List[A]]): List[A] = {
    foldRight(a1, Nil: List[A])(append)
  }

}

val xx = Cons("A", Nil)
val yy = Cons(1, Cons(2, Cons(5, Nil)))
val zz = List(1,2,3,4,5)
List.tail(zz)
List.setHead(8, zz)
List.drop(zz, 3)
List.dropWhile(zz, (x: Int) => x < 4)
List.init(zz)

List.dropWhilePlus(zz)(x => x < 4)


def product(ns: List[Double]) =
  List.foldRight(ns, 1.0)(_ * _)

def sumLeft(ns: List[Int]) =
  List.foldLeft(ns, 0)(_ + _)

sumLeft(List(1, 2, 3))


product(List(1, 2, 3, 4))

List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_, _))
List(1,2,3)
List.length(List(1,2,3,4,6))

List.reverse(List(1,2,3))

List.foldRightViaFoldLeft(List(1, 2, 3, 4), 0)(_ + _)
List.append(List(1,2,3), List(4))

List.concat(List(List(1,2,3), List(3,4,5)))