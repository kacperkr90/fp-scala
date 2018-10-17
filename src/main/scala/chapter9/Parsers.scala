package chapter9

import chapter8.Prop
import chapter8.Prop._
import chapter8.gen.Gen
import org.scalactic.ErrorMessage

import scala.util.matching.Regex

trait Parsers[Parser[+_]] {self =>

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

  def label[A](msg: String)(p: Parser[A]): Parser[A]

  def errorLocation(e: ParseError): Location
  def errorMessage(e: ErrorMessage): Location

  val p = label("first magic word")("abra") **
    " ".many **
    label("second magic word")("cadabra")

  def scope[A](msg: String)(p: Parser[A]): Parser[A]

  def attempt[A](p: Parser[A]): Parser[A]

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

case class ParseError(stack: List[(Location, String)]) {
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc,msg) :: stack)

  def label[A](s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latestLoc: Option[Location] =
    latest map (_._1)

  def latest: Option[(Location, String)] =
    stack.lastOption
}

case class Location(input: String, offset: Int = 0) {
  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1
  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match {
    case -1 => offset + 1
    case lineStart => offset - lineStart
  }

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int): Location =
    copy(offset = offset+n)
}

trait MyParsers[ParseError, Parser[+_]] {self =>

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

  def map3[A, B, C, D](p1: Parser[A], p2: Parser[B], p3: Parser[C])(f: (A, B, C) => D): Parser[D] =
    map2(p1, map2(p2, p3)((b, c) => (a: A) => f(a,b,c)))((a, g) => g(a))

  def map4[A, B, C, D, E](p1: Parser[A], p2: Parser[B], p3: Parser[C], p4: Parser[D])(f: (A, B, C, D) => E): Parser[E] =
    map3(p1, p2, map2(p3, p4)((c, d) => (a: A, b: B) => f(a, b, c, d)))((a, b, g) => g(a, b))

  def map5[A, B, C, D, E, F](p1: Parser[A], p2: Parser[B], p3: Parser[C], p4: Parser[D], p5: Parser[E])(f: (A, B, C, D, E) => F): Parser[F] =
    map4(p1, p2, p3, map2(p4, p5)((d, e) => (a: A, b: B, c: C) => f(a, b, c, d, e)))((a, b, c, g) => g(a, b, c))

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