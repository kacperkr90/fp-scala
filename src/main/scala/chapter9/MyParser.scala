package chapter9
import chapter9.MyParser.{Failure, Parser, Success}
import org.scalactic.ErrorMessage

import scala.util.matching.Regex

object MyParser {

  type Parser[+A] = Location => Result[A]

  trait Result[+A] {
    def mapError(f: ParseError => ParseError): Result[A] = this match {
      case Failure(e, b) => Failure(f(e), b)
      case _ => this
    }

    def uncommit: Result[A] = this match {
      case Failure(e, true) => Failure(e, false)
      case _ => this
    }

    def advanceSuccess(n: Int): Result[A] = this match {
      case Success(a,m) => Success(a,n+m)
      case _ => this
    }

    def addCommit(isCommitted: Boolean): Result[A] = this match {
      case Failure(e,c) => Failure(e, c || isCommitted)
      case _ => this
    }
  }
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

}

object TheParsers extends Parsers[Parser] {

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
    s => s1(s) match {
      case Failure(e, false) => s2(s)
      case r => r
    }

  override implicit def string(s: String): Parser[String] =
    scope("Failure could not parse string.")(location =>
      if (location.input.startsWith(s))
        Success(s, location.offset + s.length)
      else
        Failure(location.toError("Expected: " + s), isCommitted = true))

  override def slice[A](p: Parser[A]): Parser[String] =
    location => p(location) match {
      case Success(_, n) => Success(location.input.slice(0, n), n)
      case Failure(e, b) => Failure(e, b)
    }

  override def wrap[A](p: => Parser[A]): Parser[A] = ???

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] =
    s => p(s) match {
      case Success(a,n) => f(a)(s.advanceBy(n))
        .addCommit(n != 0)
        .advanceSuccess(n)
      case e@Failure(_,_) => e
    }

  override implicit def regex(r: Regex): Parser[String] =
    location =>  {
      def consumedStringLength(input: String, matchingString: String): Int = input.indexOf(matchingString) + matchingString.length

      val input: String = location.input
      r.findFirstIn(input)
        .map(string => Success(string, location.offset + consumedStringLength(input, string)))
        .getOrElse(Failure(location.toError("String does not match: " + r ), isCommitted = true))
    }


  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  override def errorLocation(e: ParseError): Location = ???

  override def errorMessage(e: ErrorMessage): Location = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, msg))

  override def attempt[A](p: Parser[A]): Parser[A] =
    s => p(s).uncommit
}
