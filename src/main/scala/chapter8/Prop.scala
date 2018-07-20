package chapter8

import chapter6.RNG
import chapter8.Prop.{FailedCase, SuccessCount, TestCases}
import chapter8.gen.Gen

case class Prop(run: (TestCases,RNG) => Result) {

  def &&(p: Prop): Prop = Prop {
    (n, rng) => (run(n, rng), p.run(n, rng)) match {
      case (r1, _) if r1.isFalsified => r1
      case (_, r2) if r2.isFalsified => r2
      case _ => Passed
    }
  }

  def ||(p: Prop): Prop = Prop {
    (n, rng) => (run(n, rng), p.run(n, rng)) match {
      case (r1, _) if !r1.isFalsified => Passed
      case (_, r2) if !r2.isFalsified => Passed
      case (Falsified(f1, s1), Falsified(f2, s2)) => Falsified(s"($f1, $f2)", s1 + s2)
    }
  }
}

object Prop {
  type TestCases = Int
  type MaxSize = Int
  type SuccessCount = Int
  type FailedCase = String

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n,rng) => randomStream(as)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) => try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch { case e: Exception => Falsified(buildMsg(a, e), i) }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  private def unfold[A, B](start: B)(f: B => Option[Tuple2[A,B]]): Stream[A] = f(start) match {
    case Some((elem, next)) => elem #:: unfold(next)(f)
    case None => Stream.empty
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"
}

sealed trait Result {
  def isFalsified: Boolean
}
case object Passed extends Result {
  def isFalsified = false
}
case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  def isFalsified = true
}