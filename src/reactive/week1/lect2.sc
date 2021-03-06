// recap scala collection
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

val list = List(data)

for {
  JObj(bindings) <- list
  JSeq(phones) =  bindings("phoneNumbers")
  JObj(phone) <- phones
  JStr(digits) = phone("number")
  if digits startsWith "212"
} yield (bindings("firstName"), bindings("lastName"))
