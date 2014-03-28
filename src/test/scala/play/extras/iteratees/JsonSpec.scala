import org.specs2.mutable.Specification
import play.api.libs.iteratee._
import concurrent.Await
import concurrent.duration.Duration
import play.api.libs.json._
import play.extras.iteratees._

object JsonSpec extends Specification {
  "json iteratee" should {
    "parse an empty object" in test(Json.obj())
    "parse a string" in test(Json.obj("string" -> "value"))
    "parse escaped values in a string" in test(JsString("""a\"\n\r"""))
    "parse a number" in test(Json.obj("number" -> 10))
    "parse true" in test(Json.obj("boolean" -> true))
    "parse false" in test(Json.obj("boolean" -> false))
    "parse null" in test(Json.obj("obj" -> JsNull))
    "parse an empty array" in test(Json.obj("array" -> Json.arr()))
    "parse an array with stuff in it" in test(Json.obj("array" -> Json.arr("foo", "bar")))
    "parse a complex object" in test(Json.obj(
      "string" -> "value",
      "number" -> 10,
      "boolean" -> true,
      "null" -> JsNull,
      "array" -> Json.arr(Json.obj("foo" -> "bar"), 20),
      "obj" -> Json.obj("one" -> 1, "two" -> 2, "nested" -> Json.obj("spam" -> "eggs"))
    ))
    "parse a object out of a character enumerator with newlines using Enumeratee.grouped and Concurrent.broadcast" in testJsObjectWithWhitespaces()
    "parse an array out of a character enumerator with newlines using Enumeratee.grouped and Concurrent.broadcast" in testJsArrayWithWhitespaces()
  }

  def test(json: JsValue) = {
    Await.result(Enumerator(Json.stringify(json).toCharArray) |>>> JsonIteratees.jsValue, Duration.Inf) must_== json
  }


  def testJsObjectWithWhitespaces() = {
    val data = """
    {"key1": "value1"} {"key2": "value2"}{"key3": 
    "value3"} 

    """.toCharArray
    val (en, chann) = play.api.libs.iteratee.Concurrent.broadcast[Array[Char]]
    import scala.concurrent.ExecutionContext.Implicits.global

    val res = (en &> Enumeratee.grouped(JsonIteratees.jsSimpleObject) |>>> Iteratee.getChunks)
    chann.push(data)
    chann.eofAndEnd()
    Await.result(res, Duration.Inf).mkString must_== data.filterNot(_.isWhitespace).mkString
  }

  def testJsArrayWithWhitespaces() = {
    val data = """
    [1, 3, 4,5,
    6,7
    ,10,11, [0, 0, 0]

    ]

    """.toCharArray
    val (en, chann) = play.api.libs.iteratee.Concurrent.broadcast[Array[Char]]
    import scala.concurrent.ExecutionContext.Implicits.global

    val res = (en &> Enumeratee.grouped(JsonIteratees.jsSimpleArray) |>>> Iteratee.getChunks)
    chann.push(data)
    chann.eofAndEnd()
    Await.result(res, Duration.Inf).mkString must_== data.filterNot(_.isWhitespace).mkString
  }
}
