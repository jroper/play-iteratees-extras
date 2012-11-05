package controllers

import play.api._
import libs.iteratee._
import libs.json._
import play.api.mvc._

object Application extends Controller {

  import iteratees.JsonBodyParser._
  import iteratees.JsonIteratees._
  import iteratees.JsonEnumeratees._

  // case class that we will fold the result of the parsing into
  case class Errors(id: Int = 0, errors: List[String] = Nil)

  // Map function that ignores the input, and returns an identity function to leave errors alone
  def ignore[A]: A => Errors => Errors = (_) => identity[Errors]

  // The parser
  val bodyParser = parser(
    // A JSON object enumerator, expecting keys, using the specified parsers for the values of each.
    // Each value gets mapped to a function, that will be used later to fold them into our Errors result.
    jsObject(
      // Handle the exportId as a number, and map it to a function that stores the id in Errors
      "exportId" -> jsNumber.map(id => (e: Errors) => Errors(id.value.toInt, e.errors)),
      "exportDate" -> jsNullOr(jsString).map(ignore),
      "exportUser" -> jsNullOr(jsString).map(ignore),
      // Handle the items as an array, parsing the values as objects, then using enumeratee composition,
      // parse the item, import the item, and finally collect the errors and map them to the function
      // for folding into the Errors result
      "items" -> (jsArray(jsValues(jsSimpleObject)) ><> parseItem ><> importItem
        &>> Iteratee.getChunks[String].map(errorList => (e: Errors) => Errors(e.id, errorList)))
    // Fold the error functions into an Errors result
    ) &>> Iteratee.fold[Errors => Errors, Errors](Errors())((e, f) => f(e))
  )

  // The items we want to import
  case class Item(id: Int, name: String, description: String)

  // Enumeratee that parses a JsObject into an item.  Uses a simple mapping Enumeratee.
  def parseItem: Enumeratee[JsObject, Option[Item]] = Enumeratee.map {obj =>
    for {
      id <- (obj \ "id").asOpt[Int]
      name <- (obj \ "name").asOpt[String]
      description <- (obj \ "description").asOpt[String]
    } yield Item(id, name, description)
  }

  // Enumeratee that imports items.  Uses an input mapping enumeratee, and only passes a result
  // along if there is an error
  def importItem: Enumeratee[Option[Item], String] = Enumeratee.mapInput(_ match {
    case Input.El(Some(item)) =>
      println(item)
      Input.Empty
    case Input.El(None) => Input.El("An error")
    case other => other.map(_ => "")
  })

  /**
   * Our action that uses the body parser
   */
  def bulkImport = Action(bodyParser) {
    request =>
      Ok("Imported export id " + request.body.id +
        " with the following errors:\n" + request.body.errors.mkString("\n"))
  }

  def index = Action(parser(jsSimpleObject)) {
    request => Ok(request.body)
  }


}