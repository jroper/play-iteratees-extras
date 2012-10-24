package controllers

import play.api._
import libs.iteratee.Input.Empty
import libs.iteratee.Iteratee
import libs.json.{JsString, JsValue}
import play.api.mvc._
import scalax.io.Input

object Application extends Controller {

  import JsonBodyParser._

  case class State(str: String = "", list: List[JsValue] = Nil, int: Int = 0)

  def ignore[A]: A => State => State = (_) => identity[State]

  val json = parser(jsObject(Iteratee.fold[State => State, State](State())((old, f) => f(old)),
    "int" -> jsNumber.map(int => (old:State) => State(old.str, old.list, int.value.toInt)),
    "booleanFalse" -> jsBoolean.map(ignore),
    "booleanTrue" -> jsBoolean.map(ignore),
    "string" -> jsString.map(str => (old:State) => State(str.value, old.list, old.int)),
    "blah" -> jsString.map(ignore),
    "null" -> jsNull.map(ignore),
    "obj" -> jsObject.map(ignore),
    "array" -> jsArray(Iteratee.getChunks[JsValue]).map(list => (old:State) => State(old.str, list, old.int))))


  def index = Action(json) { implicit request =>
    Ok(request.body.toString)
  }

}