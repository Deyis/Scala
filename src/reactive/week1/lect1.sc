abstract class JSON
case class JSeq(elems: List[JSON]) extends JSON
case class JObj(bindings: Map[String, JSON]) extends JSON
case class JNum(num: Double) extends JSON
case class JStr(str: String) extends JSON
case class JBool(num: Boolean) extends JSON
case object JNull extends JSON


val data = JObj(Map(
  "firstName" -> JStr("John"),
  "lastName" -> JStr("Smith"),
  "address" -> JObj(Map(
    "streetAddress" -> JStr("21 2nd Street"),
    "state" -> JStr("NY"),
    "postalCode" -> JNum(10021)
  )),
  "phoneNumbers" -> JSeq(List(
    JObj(Map("type" -> JStr("home"), "number" ->JStr("212 555-1234") )),
    JObj(Map("type" -> JStr("fax"), "number" ->JStr("646 555-4567") ))
  ))
))

def show(json:JSON): String = json match {
  case JNull => "null"
  case JStr(str) => '\"' + str + '\"'
  case JNum(num) => num.toString
  case JBool(b) => b.toString
  case JSeq(elems) => "[" + (elems map show mkString ", ") + "]"
  case JObj(bindings) =>
    val assocs = bindings map {
      case(key, value) => '\"' + key + "\": " + show(value)
    }
    "{" + (assocs mkString ", ") + "}"
}

show(data)
val f: PartialFunction[String, String] = { case "ping" => "pong" }
f("ping")
//f("pong") MatchError
f.isDefinedAt("ping")
f.isDefinedAt("abc")