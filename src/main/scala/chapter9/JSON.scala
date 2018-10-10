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

    val jnull: Parser[JSON] = string("null").map(_ => JNull)

    val jdouble: Parser[JSON] = regex("\\d+(\\.\\d+)?".r)
      .map(_.toDouble)
      .map(JNumber)

    val vstring: Parser[String] = regex("\".*\"".r)
    val jstring: Parser[JSON] = regex("\".*\"".r)
      .map(JString)

    val jbool: Parser[JSON] = ("true" | "false")
      .map(_.toBoolean)
      .map(JBool)

    val jarray

    val anyJPrimitive = jnull | jdouble | jstring | jbool |

    val entry: (String, JSON) = map5(vstring, spaces, colon(), spaces, )


  }

}
