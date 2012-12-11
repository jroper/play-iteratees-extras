package play.extras.iteratees

import play.api.libs.iteratee._
import play.api.libs.iteratee.Input._
import play.api.libs.json._
import play.api.mvc.{RequestHeader, BodyParser}
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import concurrent.Future

/**
 * Body parser for reactively parsing JSON.  Used with no arguments, it just parses a JsValue into memory.  However,
 * you can do more powerful things, for example, let's say you have a bulk load function that takes a possibly very
 * long JSON input containing the bulk data to load, including some meta data, like the following:
 *
 * {{{
 * {
 *   "exportId": 12345,
 *   "exportDate": "17/10/2012",
 *   "exportUser": "bob"
 *   "items": [
 *     { "id": 1, ... },
 *     ...
 *     ]
 * }
 * }}}
 *
 * You could parse this, without loading all the items in memory, but rather saving them to a database as they
 * arrive, like this:
 *
 * {{{
 * import JsonBodyParser._
 * import JsonIteratees._
 * import JsonEnumeratees._
 *
 * // case class that we will fold the result of the parsing into
 * case class Errors(id: Int = 0, errors: List[String] = Nil)
 *
 * // Map function that ignores the input, and returns an identity function to leave errors alone
 * def ignore[A]: A => Errors => Errors = (_) => identity[Errors]
 *
 * // The parser
 * val bodyParser = parser(
 *   // A JSON object enumerator, expecting keys, using the specified parsers for the values of each.
 *   // Each value gets mapped to a function, that will be used later to fold them into our Errors result.
 *   jsObject(
 *     // Handle the exportId as a number, and map it to a function that stores the id in Errors
 *     "exportId" -> jsNumber.map(id => (e: Errors) => Errors(id.value.toInt, e.errors)),
 *     "exportDate" -> jsNullOr(jsString).map(ignore),
 *     "exportUser" -> jsNullOr(jsString).map(ignore),
 *     // Handle the items as an array, parsing the values as objects, then using enumeratee composition,
 *     // parse the item, import the item, and finally collect the errors and map them to the function
 *     // for folding into the Errors result
 *     "items" -> (jsArray(jsValues(jsSimpleObject)) ><> parseItem ><> importItem
 *       &>> Iteratee.getChunks[String].map(errorList => (e: Errors) => Errors(e.id, errorList)))
 *   // Fold the error functions into an Errors result
 *   ) &>> Iteratee.fold[Errors => Errors, Errors](Errors())((e, f) => f(e))
 * )
 *
 * // The items we want to import
 * case class Item(id: Int, name: String, description: String)
 *
 * // Enumeratee that parses a JsObject into an item.  Uses a simple mapping Enumeratee.
 * def parseItem: Enumeratee[JsObject, Option[Item]] = Enumeratee.map {obj =>
 *   for {
 *     id <- (obj \ "id").asOpt[Int]
 *     name <- (obj \ "name").asOpt[String]
 *     description <- (obj \ "description").asOpt[String]
 *   } yield Item(id, name, description)
 * }
 *
 * // Enumeratee that imports items.  Uses an input mapping enumeratee, and only passes a result
 * // along if there is an error
 * def importItem: Enumeratee[Option[Item], String] = Enumeratee.mapInput(_ match {
 *   case Input.El(Some(item)) =>
 *     println(item)
 *     Input.Empty
 *   case Input.El(None) => Input.El("An error")
 *   case other => other.map(_ => "")
 * })
 *
 * // Our action that uses the body parser
 * def bulkImport = Action(bodyParser) {
 *   request =>
 *     Ok("Imported export id " + request.body.id +
 *       " with the following errors:\n" + request.body.errors.mkString("\n"))
 * }
 * }}}
 */
object JsonBodyParser {

  import JsonParser._

  /**
   * Create a parser
   *
   * @param handler An iteratee to handle the JSON.  By default parses them into memory as a JsValue
   */
  def parser[A](handler: Iteratee[Array[Char], A] = jsonValue) = new BodyParser[A] {
    def apply(rh: RequestHeader) = {
      Encoding.decode() ><> Combinators.errorReporter &>> handler.map(result => Right(result))
    }
  }
}

object JsonIteratees {

  import JsonParser._

  /**
   * Parse an object
   */
  def jsSimpleObject = jsonObject()

  /**
   * Parse an array
   */
  def jsSimpleArray = jsonArray()

  /**
   * Parse a string
   */
  def jsString = jsonString

  /**
   * Parse a number
   */
  def jsNumber = jsonNumber

  /**
   * Parse a boolean
   */
  def jsBoolean = jsonBoolean

  /**
   * Parse a null
   */
  def jsNull = jsonNull

  /**
   * Parse a null or something else.  If null is found, it returns None, otherwise, it
   * returns Some(A)
   */
  def jsNullOr[A](valueParser: Iteratee[Array[Char], A]): Iteratee[Array[Char], Option[A]] = jsonNullOr(valueParser)

  /**
   * Parse a generic value
   */
  def jsValue = jsonValue

  /**
   * Use to parse all the values of a map or array as a single type, as determined by the passed in parser.
   *
   * For use with JsEnumeratee.jsObject and JsEnumaretee.jsArray.
   */
  def jsValues[A, I](valueParser: Iteratee[Array[Char], A]) = (index: I) => valueParser

  /**
   * Use to parse all the values of a map into key value pairs, using the given parser to parse the value.
   *
   * For use with JsEnumeratee.jsObject.
   */
  def jsKeyValues[A](valueParser: Iteratee[Array[Char], A]) = (key: String) => valueParser.map((value) => (key, value))

  /**
   * Use to parse all the values of an array into indexed pairs, using the given parser to parse the value.
   *
   * For use with JsEnumeratee.jsArray.
   */
  def jsIndexedValues[A](valueParser: Iteratee[Array[Char], A]) = (index: Int) => valueParser.map((value) => (index, value))
}

object JsonEnumeratees {

  import JsonParser._

  /**
   * Enumeratee for a JSON object.  Adapts a stream of character arrays into a stream of key to JsValue pairs.
   */
  def jsObject: Enumeratee[Array[Char], (String, JsValue)] =
    jsObject(JsonIteratees.jsKeyValues(jsonValue))

  /**
   * Enumeratee for a JSON object.  Adapts a stream of character arrays into a stream of parsed key value
   * pairs.  The pairs are parsed according to the passed in key to iteratee mappings.
   *
   * @param valueParsers  A mapping of keys to iteratees to parse their values.  If a parser is not found for a given
   *                      key, an error will be raised.
   */
  def jsObject[V](valueParsers: (String, Iteratee[Array[Char], V])*): Enumeratee[Array[Char], V] = {
    jsObject((key: String) => Map(valueParsers:_*).get(key).getOrElse(Error("Unexpected key found in JSON: " + key, Empty)))
  }

  /**
   * Enumeratee for a JSON object.  Adapts a stream of character arrays into a stream of parsed key value
   * pairs.  The pairs are parsed according to the valueParser, which takes a key and returns an iteratee
   * to parse it's value.
   *
   * @param valueParser   A function that returns an iteratee to parse the value for a given key.
   */
  def jsObject[V](valueParser: String => Iteratee[Array[Char], V]) = new Enumeratee[Array[Char], V] {
    def step[A](inner: Iteratee[V, A])(in: Input[V]): Iteratee[V, Iteratee[V, A]] = in match {
      case EOF => Done(inner, in)
      case _ => Cont(step(Iteratee.flatten(inner.feed(in))))
    }

    def applyOn[A](inner: Iteratee[V, A]): Iteratee[Array[Char], Iteratee[V, A]] = {
      jsonObject(Cont(step(inner)), valueParser)
    }
  }

  /**
   * Enumeratee for a JSON array.  Adapts a stream of character arrays into a stream of JsValues.
   */
  def jsArray: Enumeratee[Array[Char], JsValue] = jsArray((index: Int) => jsonValue)

  /**
   * Enumeratee for a JSON array.  Adapts a stream of character arrays into a stream of parsed values
   * The values are parsed according to the passed sequence of iteratees.
   *
   * @param valueParsers  A sequence of iteratees to parse values.  If more elements are found then the
   *                      sequence contains, an error is thrown.
   */
  def jsArray[V](valueParsers: (Int, Iteratee[Array[Char], V])*): Enumeratee[Array[Char], V] = {
    val parserAt = valueParsers.lift
    jsArray((index: Int) => parserAt(index).map(_._2).getOrElse(Error("No handler provided for element " + index + " of the array", Empty)))
  }

  /**
   * Enumeratee for a JSON array.  Adapts a stream of character arrays into a stream of parsed values
   * The values are parsed according to the valueParser, which takes an array index and returns an iteratee
   * to parse it's value.
   *
   * @param valueParser   A function that returns an iteratee to parse the value for a array index.
   */
  def jsArray[V](valueParser: Int => Iteratee[Array[Char], V]) = new Enumeratee[Array[Char], V] {
    def step[A](inner: Iteratee[V, A])(in: Input[V]): Iteratee[V, Iteratee[V, A]] = in match {
      case EOF => Done(inner, in)
      case _ => {
        Cont(step(Iteratee.flatten(inner.feed(in))))
      }
    }

    def applyOn[A](inner: Iteratee[V, A]): Iteratee[Array[Char], Iteratee[V, A]] = {
      jsonArray(Cont(step(inner)), valueParser)
    }
  }

}

object JsonParser {

  import Combinators._

  /**
   * Creates a JSON object from a key value iteratee
   */
  def jsonObjectCreator: Iteratee[(String, JsValue), JsObject] = Iteratee.getChunks.map(keyValues => new JsObject(keyValues))

  /**
   * Creates a JSON array from a key value iteratee
   */
  def jsonArrayCreator: Iteratee[JsValue, JsArray] = Iteratee.getChunks.map(values => new JsArray(values))

  //
  // JSON parsing
  //

  def jsonKeyValue[A](valueHandler: String => Iteratee[Array[Char], A]) = for {
    key <- jsonString
    _ <- skipWhitespace
    _ <- expect(':')
    _ <- skipWhitespace
    value <- valueHandler(key.value)
  } yield {
    value
  }

  def jsonKeyValues[A, V](keyValuesHandler: Iteratee[V, A],
                           valueHandler: String => Iteratee[Array[Char], V]
                            ): Iteratee[Array[Char], A] = for {
    _ <- skipWhitespace
    fed <- jsonKeyValue(valueHandler).map(keyValue => Iteratee.flatten(keyValuesHandler.feed(El(keyValue))))
    _ <- skipWhitespace
    ch <- takeOneOf('}', ',')
    keyValues <- ch match {
      case '}' => Iteratee.flatten(fed.run.map((a: A) => done(a)))
      case ',' => jsonKeyValues(fed, valueHandler)
    }
  } yield keyValues


  def jsonObject[A, V](keyValuesHandler: Iteratee[V, A] = jsonObjectCreator,
                       valueHandler: String => Iteratee[Array[Char], V] = (key: String) => jsonValue.map(value => (key, value))
                        ) = for {
    _ <- expect('{')
    - <- skipWhitespace
    ch <- peekOne
    keyValues <- ch match {
      case Some('}') => drop(1).flatMap(_ => Iteratee.flatten(keyValuesHandler.run.map((a: A) => done(a))))
      case _ => jsonKeyValues(keyValuesHandler, valueHandler)
    }
  } yield keyValues

  def jsonValueForEach: Int => Iteratee[Array[Char], JsValue] = index => jsonValue

  def jsonArrayValues[A, V](valuesHandler: Iteratee[V, A],
                            valueHandler: Int => Iteratee[Array[Char], V],
                            index: Int = 0): Iteratee[Array[Char], A] = for {
    _ <- skipWhitespace
    fed <- valueHandler(index).map(value => Iteratee.flatten(valuesHandler.feed(El(value))))
    _ <- skipWhitespace
    ch <- takeOneOf(']', ',')
    values <- ch match {
      case ']' => Iteratee.flatten(fed.run.map((a: A) => done(a)))
      case ',' => jsonArrayValues(fed, valueHandler, index + 1)
    }
  } yield values


  def jsonArray[A, V](valuesHandler: Iteratee[V, A] = jsonArrayCreator,
                      valueHandler: Int => Iteratee[Array[Char], V] = jsonValueForEach) = for {
    _ <- expect('[')
    _ <- skipWhitespace
    ch <- peekOne
    values <- ch match {
      case Some(']') => for {
        _ <- drop(1)
        empty <- Iteratee.flatten(valuesHandler.run.map((a: A) => done(a)))
      } yield empty
      case _ => jsonArrayValues(valuesHandler, valueHandler)
    }
  } yield values

  def jsonValue: Iteratee[Array[Char], JsValue] = peekOne.flatMap({
    case Some('"') => jsonString
    case Some('{') => jsonObject()
    case Some('[') => jsonArray()
    case Some(n) if (n == '-' || (n >= '0' && n <= '9')) => jsonNumber
    case Some('f') | Some('t') => jsonBoolean
    case Some('n') => jsonNull
    case Some(c) => error("Expected JSON value, but found: " + c)
    case None => error("Expected JSON value, but found EOF")
  })

  def jsonNumber = for {
    number <- peekWhile(ch => ch.isDigit || ch == '+' || ch == '-' || ch == '.' || ch == 'e' || ch == 'E')
    jsNumber <- try {
      done(new JsNumber(BigDecimal(number)))
    } catch {
      case e: NumberFormatException => error("Unparsable number: " + number)
    }
    _ <- dropWhile(ch => ch.isDigit || ch == '+' || ch == '-' || ch == '.' || ch == 'e' || ch == 'E')
  } yield jsNumber

  def jsonBoolean = for {
    boolean <- peekWhile(ch => ch >= 'a' && ch <= 'z')
    jsBoolean <- boolean match {
      case t if t == "true" => done(JsBoolean(true))
      case f if f == "false" => done(JsBoolean(false))
      case o => error("Unknown identifier: " + o)
    }
    _ <- dropWhile(ch => ch >= 'a' && ch <= 'z')
  } yield jsBoolean

  def jsonNullOr[A](other: Iteratee[Array[Char], A]) = for {
    nullStr <- peekWhile(ch => ch >= 'a' && ch <= 'z')
    value <- nullStr match {
      case n if n == "null" => dropWhile(ch => ch >= 'a' && ch <= 'z').map(_ => None)
      case o => other.map(v => Some(v))
    }
  } yield value

  def jsonNull = for {
    nullStr <- peekWhile(ch => ch >= 'a' && ch <= 'z')
    jsNull <- nullStr match {
      case n if n == "null" => done(JsNull)
      case o => error("Unknown identifier: " + o)
    }
    _ <- dropWhile(ch => ch >= 'a' && ch <= 'z')
  } yield jsNull

  def jsonString: Iteratee[Array[Char], JsString] = {

    // Represents the state of the fold.  We have the result StringBuilder, possibly a remaining StringBuilder
    // if we've encountered an error, an escaped StringBuilder if we are currently reading an escape sequence,
    // and possibly an error message.  Because this is a mutable structure with the StringBuilders, we, for
    // the normal case of growing the result or remaining buffers, do not need to create a new state, but can
    // just append to the buffers.
    case class State(result: StringBuilder,
                     remaining: Option[StringBuilder],
                     escaped: Option[StringBuilder],
                     error: Option[String])

    def stringContents(sb: StringBuilder = new StringBuilder, escaped: Option[StringBuilder] = None): Iteratee[Array[Char], String] = Cont {
      case in @ EOF => Error("Unexpected end of input in the middle of a String", in)
      case Empty => stringContents(sb, escaped)
      case El(data) => {

        // Fold it
        val state = data.foldLeft(new State(sb, None, escaped, None)) {
          // We've finished, and are collecting remaining characters
          case (state @ State(_, Some(remaining), _, _), ch) => {
            remaining.append(ch)
            state
          }
          // We're in the middle of an escape sequence
          case (state @ State(result, _, Some(esc), _), ch) => {
            unescape(esc.append(ch)).fold(
              err => new State(result, Some(new StringBuilder().append("\\").append(esc)), None, Some(err)),
              _.map(c => new State(result.append(c), None, None, None)).getOrElse(state)
            )
          }
          // Control character, that's an error
          case (State(result, _, _, _), control) if control >= 0 && control < 32 => {
            new State(result, Some(new StringBuilder), None,
              Some("Illegal control character found in JSON String: 0x" + Integer.toString(control, 16)))
          }
          // We've encountered the start of an escape sequence
          case (State(result, _, _, _), '\\') => new State(result, None, Some(new StringBuilder), None)

          // We've encountered the end of the String, and should start collecting remaining characters
          case (State(result, _, _, _), '"') => new State(result, Some(new StringBuilder), None, None)
          // An ordinary character to append to the result
          case (state @ State(result, _, _, _), ch) => {
            result.append(ch)
            state
          }
        }
        // Convert the state to an Iteratee
        state match {
          case State(_, Some(remaining), _, Some(err)) => Error(err, El(remaining.toArray))
          case State(result, Some(remaining), _, _) => Done(result.toString(), El(remaining.toArray))
          case State(result, _, esc, _) => stringContents(result, esc)
        }
      }
    }

    /**
     * Unescape the escape sequence in the StringBuilder
     *
     * @param esc The escape sequence.  Must not include the leading \, and must contain at least 1 character
     * @return Either the result of the unescaping, or an error.  The result of the unescaping is some character, or
     *         None if more input is needed to unescape this sequence.
     */
    def unescape(esc: StringBuilder) : Either[String, Option[Char]] = {
      esc(0) match {
        case 'n' => Right(Some('\n'))
        case 'r' => Right(Some('\r'))
        case 't' => Right(Some('\t'))
        case 'b' => Right(Some('\b'))
        case 'f' => Right(Some('\f'))
        case '\\' => Right(Some('\\'))
        case '"' => Right(Some('"'))
        case '/' => Right(Some('/'))
        case 'u' if esc.size >= 5 => try {
          Right(Some(Integer.parseInt(esc.drop(1).toString(), 16).toChar))
        } catch {
          case e: NumberFormatException => Left("Illegal unicode escape sequence: \\u" + esc)
        }
        case 'u' => Right(None)
        case _ => Left("Unknown escape sequence: \\" + esc)
      }
    }

    for {
      _ <- expect('"')
      data <- stringContents()
    } yield new JsString(data)
  }
}
