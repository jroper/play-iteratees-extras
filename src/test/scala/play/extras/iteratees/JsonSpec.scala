package play.extras.iteratees

import play.api.libs.json._
import org.specs2.mutable.Specification
import play.api.libs.iteratee.Enumerator

object JsonSpec extends Specification {
  "json iteratee" should {
    "parse an empty object" in test(obj())
    "parse a string" in test(obj("string" -> JsString("value")))
    "parse escaped values in a string" in test(JsString("""a\"\n\r"""))
    "parse a number" in test(obj("number" -> JsNumber(10)))
    "parse true" in test(obj("boolean" -> JsBoolean(true)))
    "parse false" in test(obj("boolean" -> JsBoolean(false)))
    "parse null" in test(obj("obj" -> JsNull))
    "parse an empty array" in test(obj("array" -> arr()))
    "parse an array with stuff in it" in test(obj("array" -> arr(JsString("foo"), JsString("bar"))))
    "parse a complex object" in test(obj(
      "string" -> JsString("value"),
      "number" -> JsNumber(10),
      "boolean" -> JsBoolean(true),
      "null" -> JsNull,
      "array" -> arr(obj("foo" -> JsString("bar")), JsNumber(20)),
      "obj" -> obj("one" -> JsNumber(1), "two" -> JsNumber(2), "nested" -> obj("spam" -> JsString("eggs")))
    ))
  }

  def obj(values: (String, JsValue)*): JsObject = JsObject(values.toSeq)
  def arr(values: JsValue*): JsArray = JsArray(values.toSeq)

  def test(json: JsValue) = {
    (Enumerator(Json.stringify(json).toCharArray) |>> JsonIteratees.jsValue).flatMap(_.run).await.get must_== json
  }
}
