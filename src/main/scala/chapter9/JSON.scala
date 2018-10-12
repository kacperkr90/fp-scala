package chapter9

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def myJsonParser[Err,Parser[+_]](P: MyParsers[Err,Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice

    def surrounded[A, B](p1: Parser[A], p2: Parser[B], p3: Parser[A]): Parser[B] =
      map3(p1, p2, p3)((_, p, _) => p)

    def surroundedBySpaces[A](p: Parser[A]): Parser[A] =
      surrounded(spaces, p, spaces)

    def surrounded[A](c: Char, p: Parser[A], c2: Char): Parser[A] =
      surrounded(surroundedBySpaces(char(c)), p, surroundedBySpaces(char(c2)))

    val colon: Parser[Char] = surroundedBySpaces(char(':'))
    val comma: Parser[Char] = surroundedBySpaces(char(','))

    def JPrimitive: Parser[JSON] = jnull | jdouble | jstring | jbool | jarray | jobject

    def jnull: Parser[JSON] = string("null").map(_ => JNull)

    def jdouble: Parser[JSON] = regex("\\d+(\\.\\d+)?".r)
      .map(_.toDouble)
      .map(JNumber)

    def vstring: Parser[String] = regex("\".*\"".r)
    def jstring: Parser[JSON] = regex("\".*\"".r)
      .map(JString)

    def jbool: Parser[JSON] = ("true" | "false")
      .map(_.toBoolean)
      .map(JBool)

    def jarray: Parser[JSON] = surrounded('[', commaSeparatedJsons, ']')
      .map(array => JArray(array.toIndexedSeq))

    def commaSeparated[A](p: Parser[A]): Parser[List[A]] =
      map2(p, commaPrependedParsers(p))(_ :: _)
        .many
        .map(_.flatten)

    def commaPrependedParsers[A](p: Parser[A]): Parser[List[A]] =
      map2(comma, p)((_, a) => a).many

    def commaSeparatedJsons: Parser[List[JSON]] =
      commaSeparated(JPrimitive)

    def jobject: Parser[JSON] = surrounded('{', commaSeparatedMapEntries, '}')
      .map(_.toMap)
      .map(map => JObject(map))

    def commaSeparatedMapEntries: Parser[List[(String, JSON)]] =
      commaSeparated(mapEntry)

    def mapEntry: Parser[(String, JSON)] = map3(vstring, colon, JPrimitive)((key, _, value) => (key, value))

    jobject | jarray
  }

}
