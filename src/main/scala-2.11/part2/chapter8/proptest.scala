package part2.chapter8

import part1.chapter6.{State,RNG,Simple}

object Prop {
  type SuccessCount = Int
  type FailedCase = String
}


case class Gen[A](sample: State[RNG,A])

trait Prop {
  def check: Either[(Prop.FailedCase, Prop.SuccessCount), Prop.SuccessCount]

 // def &&(p: Prop): Prop = new Prop {
 //   //def check = Prop.this.check && p.check
 //   ???
 // }

}

object Gen {
  def listOf[A](a: Gen[A]): Gen[List[A]] = ???

  def choose(start: Int, stopExclusive: Int): Gen[Int] = {
    Gen(State((rng: RNG) => RNG.rangeInt(rng, start, stopExclusive)))
  }
}
