trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(x) => Some(f(x))
  }

  def flatMap[B](f: A => Option[B]): Option[B]  = map(f) getOrElse(None)

  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(x) => x
  }
  def orElse[B >: A](ob: => Option[B]): Option[B] = map(Some(_))  getOrElse(ob)
  def filter(f: A => Boolean): Option[A] = flatMap(x => if(f(x)) Some(x) else None)
}

object Option {
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    Try(a.map(x => x.getOrElse(throw new NullPointerException())))
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(x => sequence2(t) map(y => x :: y))
    }
  }

  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a match {
      case Nil => Some(Nil)
      case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
    }
  }
}

case object None extends Option[Nothing]
case class Some[+A](get: A) extends Option[A]

def mean(xs: Seq[Double]): Option[Double] = {
  if(xs.isEmpty) None else Some(xs.sum / xs.length)
}

def variance(xs: Seq[Double]): Option[Double] = {
  mean(xs).flatMap(m => mean(xs.map(n => Math.pow(n - m, 2))))
}

variance(Seq())
variance(Seq(1, 2, 3, 4))
variance(Seq(2, 4, 6))

def lift[A,B](f: A => B): Option[A] => Option[B] = _ map(f)


val abs0: Option[Double] => Option[Double] = lift(Math.abs)
abs0(Some(1.0))

def Try[A](a: => A): Option[A] =
  try Some(a)
  catch { case e: Exception => None}

def insuranceRateQuota(age:Int, tickets: Int): Double = {
  Math.pow(age, tickets)
}

def parseInsuranceRateQuote(age: String, tickets: String): Option[Double] = {
  val optAge = Try(age.toInt)
  val optTickets = Try(tickets.toInt)
  map2(optAge, optTickets)(insuranceRateQuota)
}

parseInsuranceRateQuote("1", "2")
parseInsuranceRateQuote("25", "2")
parseInsuranceRateQuote("1000", "20000A")

// 4.3
def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B) => C): Option[C] = {
  a.flatMap(x => b.map(y => f(x, y)))
}

val optSeq: List[Option[Int]] = List(Some(1), Some(2), Some(3))
Option.sequence(optSeq)
Option.sequence(optSeq :+ None)


Option.sequence2(optSeq)
Option.sequence2(optSeq :+ None)
Option.traverse(optSeq)(x => x.map( y => y * 2.0))