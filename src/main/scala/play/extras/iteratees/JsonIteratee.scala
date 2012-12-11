package play.extras.iteratees

import play.api.libs.iteratee._
import play.api.libs.iteratee.Input._
import play.api.libs.json._
import play.api.mvc.{RequestHeader, BodyParser}
import concurrent.ExecutionContext.Implicits.global
import concurrent.Future
import java.nio.{ByteBuffer, CharBuffer}
import java.nio.charset.{CharsetDecoder, Charset}

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
      toCharArray() ><> errorReporter &>> handler.map(result => Right(result))
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
  def jsString = jsonString().map(s => JsString(s))

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

  /**
   * Creates a JSON object from a key value iteratee
   */
  def jsonObjectCreator: Iteratee[(String, JsValue), JsObject] = Iteratee.getChunks.map(keyValues => new JsObject(keyValues))

  /**
   * Creates a JSON array from a key value iteratee
   */
  def jsonArrayCreator: Iteratee[JsValue, JsArray] = Iteratee.getChunks.map(values => new JsArray(values))

  //
  // Helper functions
  //

  /**
   * Error iteratee.  Differs from Iteratees Error iteratee in that initially this one is in the cont state, so that
   * it can get the remaining input to pass back in the error.  This makes it more useful for composition.
   */
  def error[A](msg: String) = new Iteratee[Array[Char], A] {
    def fold[B](folder: (Step[Array[Char], A]) => Future[B]) = {
      folder(Step.Cont { input =>
        Error(msg, input)
      })
    }
  }

  /**
   * Done iteratee, typed with Array[Char] to avoid needing to type it ourselves
   */
  def done[A](a: A) = Done[Array[Char], A](a)

  def FailOnEof[E, A](f: E => Iteratee[E, A]): Iteratee[E, A] = Cont {
    case in @ EOF => Error("Premature end of input", in)
    case Empty => FailOnEof(f)
    case El(data) => f(data)
  }

  def skipWhitespace = dropWhile(_.isWhitespace)

  def expect(value: Char) = for {
    ch <- peekOne
    result <- ch match {
      case Some(c) if c == value => done(Unit)
      case Some(c) => error("Expected '" + value + "' but got '" + c + "'")
      case None => error("Premature end of input, expected '" + value + "'")
    }
    _ <- drop(1)
  } yield result

  def drop(n: Int): Iteratee[Array[Char], Unit] = Cont {
    case in @ EOF => Done(Unit, in)
    case Empty => drop(n)
    case El(data) => {
      val remaining = n - data.length
      if (remaining == 0) {
        Done(Unit, Empty)
      } else if (remaining < 0) {
        Done(Unit, El(data.drop(n)))
      } else {
        drop(remaining)
      }
    }
  }

  def takeOneOf(values: Char*) = for {
    ch <- peekOne
    result <- ch match {
      case Some(c) if values.contains(c) => done(c)
      case Some(c) => error("Expected one of " + values.mkString("'", "', '", "'") + " but got '" + c + "'")
      case None => error("Premature end of input, expected one of " + values.mkString("'", "', '", "'"))
    }
    _ <- drop(1)
  } yield result

  def dropWhile(p: Char => Boolean): Iteratee[Array[Char], Unit] = Cont {
    case in @ EOF => Done(Unit, in)
    case Empty => dropWhile(p)
    case El(data) => {
      val dropped = data.dropWhile(p)
      if (dropped.length == 0) {
        dropWhile(p)
      } else {
        Done(Unit, El(dropped))
      }
    }
  }

  def peekWhile(p: Char => Boolean, peeked: Array[Char] = Array[Char]()): Iteratee[Array[Char], String] = Cont {
    case in @ EOF => Done(new String(peeked), El(peeked))
    case Empty => peekWhile(p, peeked)
    case El(data) => {
      val taken = data.takeWhile(p)
      if (taken.length == data.length) {
        peekWhile(p, peeked ++ taken)
      } else {
        Done(new String(peeked ++ taken), El(peeked ++ data))
      }
    }
  }

  def takeOne(expected: => String): Iteratee[Array[Char], Char] = FailOnEof { data =>
    data.headOption.map(c => Done(c, El(data.drop(1)))).getOrElse(takeOne(expected))
  }

  def peekOne = new Iteratee[Array[Char], Option[Char]] {
    def fold[B](folder: (Step[Array[Char], Option[Char]]) => Future[B]) = {
      folder(Step.Cont {
        case in @ EOF => Done(None, in)
        case Empty => this
        case in @ El(data) => {
          data.headOption.map(c => Done(Some(c), in)).getOrElse(this)
        }
      })
    }
  }

  //
  // JSON parsing
  //

  def jsonKeyValue[A](valueHandler: String => Iteratee[Array[Char], A]) = for {
    key <- jsonString()
    _ <- skipWhitespace
    _ <- expect(':')
    _ <- skipWhitespace
    value <- valueHandler(key)
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
    case Some('"') => jsonString().map(s => new JsString(s))
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

  def jsonString(sb: StringBuilder = new StringBuilder, escaped: Option[StringBuilder] = None): Iteratee[Array[Char], String] =
    expect('"').flatMap(_ => new Iteratee[Array[Char], String] {
      def fold[B](folder: (Step[Array[Char], String]) => Future[B]) = {
        folder(Step.Cont {
          case in @ EOF => Error("Unexpected end of input in the middle of a String", in)
          case Empty => this
          case El(data) => {
            // Represents the state of the fold.  We have the result StringBuilder, possibly a remaining StringBuilder
            // if we've encountered an error, an escaped StringBuilder if we are currently reading an escape sequence,
            // and possibly an error message.  Because this is a mutable structure with the StringBuilders, we, for
            // the normal case of growing the result or remaining buffers, do not need to create a new state, but can
            // just append to the buffers.
            case class State(result: StringBuilder,
                             remaining: Option[StringBuilder],
                             escaped: Option[StringBuilder],
                             error: Option[String])

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
              case State(result, _, esc, _) => jsonString(result, esc)
            }
          }
        })
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
    })

  /**
   * Reports errors, attempting to find the line and column number of the error
   */
  def errorReporter: Enumeratee[Array[Char], Array[Char]] = new Enumeratee[Array[Char], Array[Char]] {

    /**
     * The current state of the error reporter
     *
     * @param chunks How many chunks have we seen?
     * @param chars How many characters have we seen?
     * @param lines How many lines have we seen?
     * @param data What was the last piece of data that we saw?
     */
    case class State(chunks: Int = 0,
      chars: Int = 0,
      lines: Int = 1,
      data: Array[Char] = Array[Char]())

    def step[A](inner: Iteratee[Array[Char], A], state: State = State())(in: Input[Array[Char]]): Iteratee[Array[Char], Iteratee[Array[Char], A]] = {
      in match {
        case El(data) => {
          // Increment chunks, chars and lines and capture data
          val nextState = new State(state.chunks + 1, state.chars + data.length, state.lines + data.count(_ == '\n'), data)
          // Fold the input into inner, mapping the input function with our error handler
          inner.pureFlatFold {
            case Step.Cont(k) => Cont(step(mapErrors(nextState, k(in)), nextState))
            case _ => Done(inner, in)
          }
        }
        case Empty => {
          // Increment chunks
          val nextState = new State(state.chunks + 1, state.chars, state.lines, state.data)
          // Fold the input into inner, mapping the input function with our error handler
          inner.pureFlatFold {
            case Step.Cont(k) => Cont(step(mapErrors(nextState, k(in)), nextState))
            case _ => Done(inner, in)
          }
        }
        // Nothing to do for EOF
        case EOF => Done(inner, in)
      }
    }

    /**
     * An iteratee that wraps another iteratee, handling the errors from that iteratee
     */
    def mapErrors[A](state: State, toMap: Iteratee[Array[Char], A]): Iteratee[Array[Char], A] = new Iteratee[Array[Char], A] {
      def fold[B](folder: (Step[Array[Char], A]) => Future[B]) = {
        toMap.fold {
          // Handle the error before passing it to the folder
          case Step.Error(msg, remaining) => folder(handleError(state, msg, remaining))
          // More input? Map this ones errors too
          case Step.Cont(k) => folder(Step.Cont(in => mapErrors(state, k(in))))
          // Done? No errors to map.
          case s => folder(s)
        }
      }
   }

    def handleError(state: State, msg: String, remainingInput: Input[Array[Char]]): Step.Error[Array[Char]] = {
      // Get the remaining input
      val remaining = remainingInput match {
        case El(data) => data
        case _ => Array[Char]()
      }
      // Maybe remaining is greater than the size of our state?  Shouldn't be, bad Iteratee.  If it is, use remaining
      // instead
      val contextData = if (remaining.length > state.data.length) remaining else state.data

      // Work out the line number the error happened on
      val lineNo = state.lines - remaining.count(_ == '\n')

      // Attempt to find the line that the error happened on
      val errorPos = contextData.length - remaining.length
      val linePos = contextData.lastIndexWhere(_ == '\n', errorPos - 1)
      val newMsg = if (linePos >= 0) {
        // We have the entire line in our context data, display an error message that includes the column number
        val line = contextData.drop(linePos + 1).takeWhile(_ != '\n')
        val col = errorPos - linePos
        """
          |Error encountered on line %d column %d: %s
          |%s
          |%s^
        """.stripMargin.format(lineNo, col, msg, new String(line), new String(line.take(col - 1).map(ch => if (ch == '\t') '\t' else ' ')))
      } else {
        // We don't have the entire line in our context data, don't report the column number as this will confuse
        val line = contextData.takeWhile(_ != '\n')
        """
          |Error encountered on line %d: %s
          |%s
          |%s^
        """.stripMargin.format(lineNo, msg, new String(line),
          new String(line.take(errorPos - 1).map(ch => if (ch == '\t') '\t' else ' ')))
      }
      Step.Error(newMsg, El(remaining))
    }

    def applyOn[A](inner: Iteratee[Array[Char], A]) = Cont(step(inner))
  }

  /**
   * Converts a stream of byte arrays to a stream of char arrays, taking into account the possibility of split multi
   * byte characters.
   *
   * @param decoder The decoder to use.  Defaults to a UTF-8 decoder
   */
  def toCharArray(decoder: CharsetDecoder = Charset.forName("UTF-8").newDecoder()): Enumeratee[Array[Byte], Array[Char]] = new Enumeratee[Array[Byte],Array[Char]] {
    /**
     * We carry partialChar as state from the stream
     */
    def step[A](inner: Iteratee[Array[Char], A], partialChar: Option[Array[Byte]] = None)(in: Input[Array[Byte]]): Iteratee[Array[Byte], Iteratee[Array[Char], A]] = {
      in match {
        // We've reached EOF. If we're in the middle of reading a multibyte character, then this is an error,
        // otherwise we just pass it down the chain
        case EOF => partialChar.map(_ => Error[Array[Byte]]("EOF encountered mid character", EOF))
          .getOrElse(Done[Array[Byte],Iteratee[Array[Char],A]](inner, EOF))

        case Empty => Cont(step(inner, partialChar))

        case El(data) => {
          // Allocate buffers

          // For char buffer, the maximum amount of space we need is the number of incoming bytes + 1.
          // The +1 here is very important, it is there for the case when there are 3 bytes of a 4 byte character
          // in the partialChar array, and so this data should contain the final byte, but that one byte will become
          // 2 Chars.
          // Performance note: we could probably reuse char buffers, passing them through the step function and
          // increasing size if necessary.
          val charBuffer = CharBuffer.allocate(data.length + 1)

          // Byte buffer should contain any left over characters followed by the input data
          val byteBuffer = partialChar.map({ leftOver =>
            val buffer = ByteBuffer.allocate(leftOver.length + data.length)
            buffer.mark()
            buffer.put(leftOver).put(data)
            buffer.reset()
            buffer
          }).getOrElse(ByteBuffer.wrap(data))

          // Decode it
          decoder.decode(byteBuffer, charBuffer, false)

          // If there are characters left in the byte buffer, this indicates that those characters are a part of a
          // multi byte character, but not the whole thing.  Store them.
          val leftOver = if (byteBuffer.limit() > byteBuffer.position()) {
            Some(byteBuffer.array().drop(byteBuffer.position()))
          } else None

          // Extract and translate to input for the iteratee
          val decoded = charBuffer.array().take(charBuffer.position())
          val input = if (decoded.length == 0) Empty else El(decoded)

          // Fold the input into the iteratee, returning it this function with the new left over state
          inner.pureFlatFold {
            case Step.Cont(k) => Cont(step(k(input), leftOver))
            case _ => Done(inner, Input.Empty)
          }
        }
      }
    }

    def applyOn[A](inner: Iteratee[Array[Char], A]) = Cont(step(inner))
  }

}
