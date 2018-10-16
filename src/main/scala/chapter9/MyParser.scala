package chapter9
import chapter9.MyParser.{Failure, Parser, Success}
import org.scalactic.ErrorMessage

import scala.util.matching.Regex

object MyParser {

  type Parser[+A] = Location => Result[A]

  trait Result[+A]
  case class Success[+A](get: A, charsConsumed: Int) extends Result[A]
  case class Failure(get: ParseError) extends Result[Nothing]

}

object TheParsers extends Parsers[Parser] {

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] = ???

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] = ???

  override implicit def string(s: String): Parser[String] =
    location =>
      if (location.input.startsWith(s))
        Success(s, location.offset + s.length)
      else
        Failure(location.toError("Expected: " + s))

  override def slice[A](p: Parser[A]): Parser[String] =
    location => p(location) match {
      case Success(_, n) => Success(location.input.slice(0, n), n)
      case Failure(e) => Failure(e)
    }

  override def wrap[A](p: => Parser[A]): Parser[A] = ???

  override def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B] = ???

  override implicit def regex(r: Regex): Parser[String] =
    location =>  {
      def consumedStringLength(input: String, matchingString: String): Int = input.indexOf(matchingString) + matchingString.length

      val input: String = location.input
      r.findFirstIn(input)
        .map(string => Success(string, location.offset + consumedStringLength(input, string)))
        .getOrElse(Failure(location.toError("String does not match: " + r )))
    }


  override def label[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def errorLocation(e: ParseError): Location = ???

  override def errorMessage(e: ErrorMessage): Location = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] = ???

  override def attempt[A](p: Parser[A]): Parser[A] = ???
}
