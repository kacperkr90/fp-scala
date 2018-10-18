package chapter9

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON]) extends JSON

  def myJsonParser[Err, Parser[+_]](P: Parsers[Parser]): Parser[JSON] = {
    import P._
    val spaces: Parser[String] = "\\s*".r

    def surrounded[A, B](p1: Parser[A], p2: Parser[B], p3: Parser[A]): Parser[B] =
      map3(p1, p2, p3)((_, p, _) => p)

    def surroundedBySpaces[A](p: Parser[A]): Parser[A] =
      surrounded(spaces, p, spaces)

    def surroundedByChar[A](c: Char, p: Parser[A], c2: Char): Parser[A] =
      surrounded(surroundedBySpaces(char(c)), p, surroundedBySpaces(char(c2)))

    val colon: Parser[Char] = surroundedBySpaces(char(':'))
    val comma: Parser[Char] = surroundedBySpaces(char(','))

    def JPrimitive: Parser[JSON] = attempt(jNull) | attempt(jBool) | attempt(jDouble) | attempt(jString) | attempt(jArray) | jObject

    def jNull: Parser[JSON] = string("null")
      .map(_ => JNull)

    def jDouble: Parser[JSON] = regex("\\d+(\\.\\d+)?".r)
      .map(_.toDouble)
      .map(JNumber)

    def vString: Parser[String] = "\".*\"".r
    def jString: Parser[JSON] = regex("\".*\"".r)
      .map(JString)

    def jBool: Parser[JSON] = ("\"true\"" | "\"false\"")
      .map(s => s.replaceAll("\"", ""))
      .map(_.toBoolean)
      .map(JBool)

    def emptyJArray: Parser[JSON] = surrounded(char('['), spaces, char(']'))
      .map(_ => JArray(IndexedSeq()))

    def jArray: Parser[JSON] = attempt(surroundedByChar('[', commaSeparatedJsons, ']')
      .map(array => JArray(array.toIndexedSeq))) | emptyJArray

    def commaSeparated[A](p: Parser[A]): Parser[List[A]] =
      map2(p, commaPrependedParsers(p))(_ :: _)

    def commaPrependedParsers[A](p: Parser[A]): Parser[List[A]] =
      map2(comma, p)((_, a) => a).many

    def commaSeparatedJsons: Parser[List[JSON]] =
      commaSeparated(JPrimitive)

    def emptyJObject: Parser[JSON] = surroundedByChar('{', spaces, '}')
      .map(_ => JObject(Map()))

    def jObject: Parser[JSON] = attempt(surroundedByChar('{', commaSeparatedMapEntries, '}')
      .map(_.toMap)
      .map(map => JObject(map))) | emptyJObject

    def commaSeparatedMapEntries: Parser[List[(String, JSON)]] =
      commaSeparated(mapEntry)

    def mapEntry: Parser[(String, JSON)] = map3(vString, colon, JPrimitive)((key, _, value) => (key, value))

    attempt(jArray) | jObject
  }

}
