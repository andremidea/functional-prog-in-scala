import scala.annotation.tailrec

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

  //3.18
  def map[A,B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
//    case Cons(x, xs) => Cons[B](f(x), map(xs)(f)) , not tailrec
    case Cons(x, xs) => foldLeft(reverse(as), Nil: List[B])((l, a) => Cons(f(a), l))
  }

  def map2[A,B](as: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]

    def append(tail: List[A]): Unit = {
      tail match {
        case Nil => Nil
        case Cons(x, xs) => {
          buf append f(x)
          append(xs)
        }
      }
    }

    append(as)
    List(buf:_*)
  }


  //3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Nil => Nil
      case Cons(x, xs) if f(x) => Cons(x, filter(xs)(f))
      case Cons(x, xs) => filter(xs)(f)
    }
  }

  def filter2[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRightViaFoldLeft(as, Nil: List[A])((h, t) => if(f(h)) Cons(h, t) else t)
  }


  //3.20
  def flapMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRightViaFoldLeft(as, Nil: List[B])((h, j) => {
      foldRightViaFoldLeft(f(h), j)(Cons(_, _))
    })
  }

  def flapMap2[A,B](as: List[A])(f: A => List[B]): List[B] = {
    concat(map(as)(f))
  }

  //3.21
  def filterUsingFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flapMap(as)((h) => if (f(h)) Cons(h, Nil) else Nil)
  }

  //3.22
  def addAll(as: List[Int], az: List[Int]): List[Int] = {
    (as, az) match {
      case (Nil, _) => Nil
      case (_, Nil) =>  Nil
      case (Cons(x, y), Cons(h, j)) => Cons[Int](x + h, addAll(y, j))
    }
  }

  //3.23
  def zipWith[A, B](as: List[A], bs: List[A])(f: (A, A) => B): List[B] = {
    (as, bs) match {
      case (Nil, _) => Nil
      case (_, Nil) =>  Nil
      case (Cons(x, y), Cons(h, j)) => Cons(f(x, h), zipWith(y, j)(f))
    }
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

// 3.16
List.map(zz)(_ + 1)

//3.17
List.map(List(1.0, 2.0, 3.0))(_.toString)
List.map2(zz)(_ + 1)
List.map2(List(1.0, 2.0, 3.0))(_.toString)

//3.19
List.filter(zz)(_ > 2)
List.filter(zz)(_ < 2)
List.filter2(zz)(_ > 2)

//3.20
List.flapMap(zz)(i => List(i, i))
List.flapMap2(zz)(i => List(i, i))

//3.21
List.filterUsingFlatMap(zz)(_ > 2)

//3.22
List.addAll(zz, zz)

//3.23
List.zipWith(zz, zz)(_ + _)

import scala.collection.immutable.{List => SList}

val reallist:SList[Int] = SList(1, 2, 3)


// 3.24 - Hard o.O
def hasSubsequence[A](sup: SList[A], sub: SList[A]): Boolean = {
  def checkList[A](sup: SList[A], sub: SList[A], firstPosition: Int): Boolean = {
    val zipped = sup.splitAt(firstPosition)._2.zip(sub)
    val result: SList[Boolean] = for {
      (a, b) <- zipped
    } yield (a == b)
    val contains: Boolean = result.forall(_ == true)
    contains
  }

  (sup, sub) match {
    case (x :: xs, y :: ys) if (sup.indexOf(y) >= 0) => checkList(sup, sub, sup.indexOf(y))
    case _ => false
  }
}


hasSubsequence(SList(1, 2, 3, 4), SList(2,3))
hasSubsequence(SList(1, 2, 3, 4), SList(2,4))
hasSubsequence(SList(1, 2, 3, 4, 5, 9), SList(5,9))

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def count[A](tree: Tree[A]): Int = tree match {
    case Leaf(x) => 1
    case Branch(x, y) => 1 + count(x) + count(y)
    case _ => 0
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(x, y) => max(x).max(max(y))
  }


  def depth[A](tree: Tree[A]): Int = tree match {
      case Leaf(x) => 1
      case Branch(x, y) => 1 + depth(x).max(depth(y))
      case _ => 0
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] = {
    tree match {
      case Leaf(x) => Leaf(f(x))
      case Branch(x, y) => Branch(map(x)(f), map(y)(f))
    }
  }

  def fold[A,B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    tree match {
      case Leaf(x) => f(x)
      case Branch(x, y) => g(fold(x)(f)(g), fold(y)(f)(g))
    }
  }
}

val t = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Branch(Leaf(5), Leaf(6))))
Tree.count(t)
Tree.max(t)
Tree.depth(t)
Tree.map(t)(_.toString)

// 3.29
Tree.fold(t)(_ => 1)(1 + _ + _)
Tree.fold(t)((x: Int) => x)(_.max(_))
Tree.fold(t)(_ => 1)(1 + _.max(_))
val mapTree: Int => Tree[String]  = x => Leaf(x.toString)
Tree.fold(t)(mapTree)((x: Tree[String], y: Tree[String]) => Branch(x, y))

val mapTree2: Int => Tree[Double]  = x => Leaf(x * 2.0)
Tree.fold(t)(mapTree2)((x: Tree[Double], y: Tree[Double]) => Branch(x, y))
