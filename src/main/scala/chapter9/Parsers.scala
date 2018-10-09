package chapter9

import chapter8.Prop
import chapter8.gen.Gen
import Prop._

import scala.util.matching.Regex

trait Parsers[ParseError, Parser[+_]] {self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char] = string(c.toString).map(_.charAt(0))
  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
  run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")

  implicit def string(s: String): Parser[String]

  run(string("abracadabra"))("abracadabra") == Right("abracadabra")

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n > 0) map2(p, listOfN(n - 1, p))(_ :: _)
    else succeed(List(): List[A])

  run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) | succeed(List(): List[A])

  def map[A, B](p: Parser[A])(f: A => B): Parser[B] =
    p.flatMap(a => succeed(f(a)))

  val numA: Parser[Int] = char('a').many.slice.map(_.length)
  run(numA)("aaa") == Right(3)
  run(numA)("b") == Right(0)

  def succeed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def slice[A](p: Parser[A]): Parser[String]
  
  run(slice(("a" | "b").many))("aaba") == Right("aaba")

  def product[A, B](p: Parser[A], p2: => Parser[B]): Parser[(A, B)] =
    p.flatMap(a => p2.flatMap(b => succeed((a, b))))

  def map2[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    p1.flatMap(a => p2.flatMap(b => succeed(f(a, b))))

  def map2ViaProduct[A, B, C](p1: Parser[A], p2: => Parser[B])(f: (A, B) => C): Parser[C] =
    (p1 ** p2).map(f.tupled)

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def productViaMap2[A, B](p: Parser[A], p2: Parser[B]): Parser[(A, B)] =
    map2(p, p2)((_, _))

  def wrap[A](p: => Parser[A]): Parser[A]

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def many: Parser[List[A]] = self.many(p)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def slice: Parser[String] = self.slice(p)
    def **[B](p2: Parser[B]): Parser[(A, B)] = self.product(p, p2)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {
    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(a => a))(in)

    def succeedLaw[A](a: A)(in: Gen[String]): Prop =
      forAll(in)(s => run(succeed(a))(s) == Right(a))

    def sliceLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p.many.slice)(s) == Right(s))

    def productLaw[A, B](p1: Parser[A], p2: Parser[B])(in: Gen[String]): Prop =
      forAll(in)(s => {
        val v1 = run(p1)(s)
        val v2 = run(p2)(s)
        val r = run(p1 ** p2)(s)
        (v1.isLeft && v2.isLeft && r.isLeft) || (Right((v1, v2)) == r)
      })

    def flatMapLaw[A, B](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.flatMap(_ => p))(in)

    def stringLaw(in: Gen[String]): Prop =
      forAll(in)(s => run(string(s))(s) == Right(s))

    def orLaw[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => {
        val v1 = run(p1)(s)
        val v2 = run(p2)(s)
        val r = run(p1 | p2)(s)
        (v1.isLeft && v2.isLeft && r.isLeft) || (v1 == r) || (v2 == r)
      })

    def regexLaw(r: Regex)(in: Gen[String]): Prop =
      forAll(in)(s => r
        .findFirstIn(s)
        .forall(m => run(regex(r))(s) == Right(m))
      )
  }
}

trait MyParsers[ParseError, Parser[+_]] {self =>

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]
  def char(c: Char): Parser[Char]
  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A]

  run(or(string("abra"),string("cadabra")))("abra") == Right("abra")
  run(or(string("abra"),string("cadabra")))("cadabra") == Right("cadabra")

  implicit def string(s: String): Parser[String]

  run(string("abracadabra"))("abracadabra") == Right("abracadabra")

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)
  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  run(listOfN(3, "ab" | "cad"))("ababcad") == Right("ababcad")
  run(listOfN(3, "ab" | "cad"))("cadabab") == Right("cadabab")
  run(listOfN(3, "ab" | "cad"))("ababab") == Right("ababab")

  def recognitions[A](p: Parser[A]): Parser[List[A]]
  def map[A,B](p: Parser[A])(f: A => B): Parser[B]

  def numberOfA(): Parser[Int] = char('a').recognitions.map(_.size)

  run(numberOfA())("aa") == Right(2)
  run(numberOfA())("") == Right(0)
  run(numberOfA())("b123") == Right(0)

  def int(a: Int): Parser[Int]
  def asError(s: String): ParseError
  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]
  def numberOfA1(): Parser[Int] = numberOfA().flatMap(i =>
    if (i < 1) int(i)
    else failed(int(i))(asError("Expected one or more 'a'")))

  run(numberOfA1())("aa") == Right(2)
  run(numberOfA1())("") == Left("Expected one or more 'a'")
  run(numberOfA1())("b123") == Left("Expected one or more 'a'")

  def failed[A](p: Parser[A])(error: ParseError): Parser[A]

  def aFollowedByB(): Parser[(Int, Int)]
//    (char('a') | char('b'))
//    .recognitions
//    .map(list => list.foldRight((((0, 0), true)))((char, tuple) => if (!tuple._2) tuple._2) else )

  run(aFollowedByB())("bbb") == Right((0, 3))
  run(aFollowedByB())("aaaab") == Right((4, 1))

  case class ParserOps[A](p: Parser[A]) {
    def |[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def or[B>:A](p2: Parser[B]): Parser[B] = self.or(p, p2)
    def map[B](f: A => B): Parser[B] = self.map(p)(f)
    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
    def recognitions: Parser[List[A]] = self.recognitions(p)
    def failed(error: ParseError): Parser[A] = self.failed(p)(error)
  }
}