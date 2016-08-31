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
  for {
    x <- optAge
    y <- optTickets
  } yield insuranceRateQuota(x, y)
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



// Either!
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Right(x) => Right(f(x))
    case Left(x) => Left(x)
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A] (b: => Either[EE, B]): Either[EE, B] = this match {
    case Right(x) => Right(x)
    case Left(e) => b
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
  {
    for {
      a <- this
      ba <- b
    } yield f(a, ba)
  }
}
case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] = {
    traverse(es)(x => x)
  }


  def traverse[E, A, B](as: List[A])(f: A=> Either[E, B]): Either[E, List[B]] = {
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h) flatMap (x => traverse(t)(f).map(l => x :: l))
    }
  }
}

def TryE[A](a: => A): Either[Exception, A] =
  try Right(a)
  catch {case e: Exception => Left(e)}

def parserInsuranceRate2(age: String, tickets: String): Either[Exception, Double] =
  for {
    a <- TryE(age.toInt)
    b <- TryE(tickets.toInt)
  } yield insuranceRateQuota(a, b)

val eithers: List[Either[Exception, Double]] = List(Right(2.0), Right(3.0), Right(4.0))
Either.traverse(eithers)(x => x.map(_ + 2.0))
Either.sequence(eithers)
Either.sequence(eithers :+ Left(new Exception("aa")))

// 4.8
case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

def mkName(name: String): Either[String, Name] =
  if (name == "" || name == null) Left("Name is empty.")
  else Right(new Name(name))

def mkAge(age: Int): Either[String, Age] =
  if (age < 0) Left("Age is out of range.")
  else Right(new Age(age))

def mkPerson(name: String, age: Int): Either[String, Person] =
  mkName(name).map2(mkAge(age))(Person(_, _))

mkPerson("fuu", 21)
mkPerson("fuu", -21)
mkPerson("", -21)
