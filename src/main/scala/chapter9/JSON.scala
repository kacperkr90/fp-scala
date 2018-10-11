package chapter9

trait JSON
object JSON {
  case object JNull extends JSON
  case class JNumber(get: Double) extends JSON
  case class JString(get: String) extends JSON
  case class JBool(get: Boolean) extends JSON
  case class JArray(get: IndexedSeq[JSON]) extends JSON
  case class JObject(get: Map[String, JSON])

  def jsonParser[Err,Parser[+_]](P: MyParsers[Err,Parser]): Parser[JSON] = {
    import P._
    val spaces = char(' ').many.slice
    val colon: Parser[Char] = char(':')
    val comma: Parser[Char] = char(',')
    def surrounded[A](c: Char, p: Parser[A], c2: Char): Parser[A] =
      map3(char(c), p, char(c2))((_, pp, _) => pp)

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

    def commaSeparatedJsons = map2(anyJPrimitive, jobjectContinuation)(_ :: _).many // TODO consider NO jsons

    def jobjectContinuation: Parser[List[JSON]] = map2(comma, anyJPrimitive)((_, json) => json).many

    def jarray: Parser[JSON] = surrounded('[', commaSeparatedJsons, ']')
      .map(b =>
        b.headOption

      )


    def jobject: Parser[JSON]

    def anyJPrimitive: Parser[JSON] = jnull | jdouble | jstring | jbool | jarray | jobject


  }

}
