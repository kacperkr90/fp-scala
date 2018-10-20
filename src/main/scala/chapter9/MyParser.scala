package chapter9
import chapter9.MyParser._
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
      case Failure(e, true) => Failure(e, isCommitted = false)
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

  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int = {
    val str = s1.substring(offset)
    if (str.startsWith(s2))
      return -1

    val a1 = str.toCharArray
    val a2 = s2.toCharArray
    a1.zipAll(a2, null, null)
      .zipWithIndex
      .find { case ((c1, c2), _) => c1 != c2 }
      .map(_._2)
      .getOrElse(s1.length)
  }
}

object TheParsers extends Parsers[Parser] {

  def surrounded[A, B](p1: Parser[A], p2: Parser[B], p3: Parser[A]): Parser[B] =
    map3(p1, p2, p3)((_, p, _) => p)

  override def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    p(Location(input)) match {
      case Success(a, _) => Right(a)
      case Failure(e, _) => Left(e)
    }

  override def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A] =
    s => s1(s) match {
      case Failure(e, false) => s2(s)
      case r => r
    }

  override implicit def string(s: String): Parser[String] =
    location => {
      val i = firstNonmatchingIndex(location.input, s, location.offset)
      if (i == -1) // they matched
        Success(s, s.length)
      else{
        val l = location.advanceBy(i)
        Failure(l.toError("Expected '" + s + "' at line " + l.line + ", column " + l.col), i != 0)
      }
    }

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

  override implicit def regex(r: Regex): Parser[String] = {
    val msg = "regex " + r
    s => r.findPrefixOf(s.input.substring(s.offset)) match {
      case None => Failure(s.toError(msg + " at line " + s.line + ", column " + s.col + ", offset " + s.offset), isCommitted = false)
      case Some(m) => Success(m,m.length)
    }
  }


  override def label[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.label(msg))

  override def errorLocation(e: ParseError): Location = ???

  override def errorMessage(e: ErrorMessage): Location = ???

  override def scope[A](msg: String)(p: Parser[A]): Parser[A] =
    s => p(s).mapError(_.push(s, msg))

  override def attempt[A](p: => Parser[A]): Parser[A] =
    s => p(s).uncommit

  override def many[A](p: Parser[A]): Parser[List[A]] =
    s => {
      var nConsumed: Int = 0
      val buf = new collection.mutable.ListBuffer[A]
      def go(p: Parser[A], offset: Int): Result[List[A]] = {
        p(s.advanceBy(offset)) match {
          case Success(a,n) => buf += a; go(p, offset+n)
          case f@Failure(e,true) => f
          case Failure(e,_) => Success(buf.toList,offset)
        }
      }
      go(p, 0)
    }

  override def succeed[A](a: A): Parser[A] =
    _ => Success(a, 0)
}

object Program {
  def main(args: Array[String]): Unit = {
    val parsers = TheParsers
    val parser = JSON.myJsonParser(parsers)

    val jsonTxt = """{"Company name" : "Microsoft Corporation", "Ticker"  : "MSFT",  "Active"  : true,  "Price"   : 30.66,  "Shares outstanding" : 8.38,  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG"]}"""

    val jsonTxt2 =
      """ {"Company name" : "Microsoft Corporation", "gsd" : "d" }""".stripMargin
    println(parsers.run(parser)(jsonTxt))
  }
}