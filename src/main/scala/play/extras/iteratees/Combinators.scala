package play.extras.iteratees

import play.api.libs.iteratee._
import scala.concurrent.{ExecutionContext, Future}
import play.api.libs.iteratee.Error
import scala.Some
import play.api.libs.iteratee.Input.{El, Empty, EOF}
import play.api.libs.iteratee.Execution.Implicits.trampoline

/**
 * Combinators for parsing using iteratees.
 *
 * Note that all the combinators that take callbacks execute them in the same thread that invokes the iteratee.
 * This means it is important that none of the callbacks ever block.
 */
object Combinators {
  /**
   * Error iteratee.  Differs from Iteratees Error iteratee in that initially this one is in the cont state, so that
   * it can get the remaining input to pass back in the error.  This makes it more useful for composition.
   */
  def error[A](msg: String) = Cont[Array[Char], A](input => Error(msg, input))

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
      case Some(c) => error("Expected '" + value + "' but got '"+ c +"' / chr(" + c.toInt + ")")
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

  val peekOne: Iteratee[Array[Char], Option[Char]] = Cont {
    case in @ EOF => Done(None, in)
    case Empty => peekOne
    case in @ El(data) => {
      data.headOption.map(c => Done(Some(c), in)).getOrElse(peekOne)
    }
  }

  /**
   * Enumeratee that keeps track of input seen, and when an error is encountered, modifies the error message so that
   * it contains the line/column number, and if possible outputs the specific line that the error occured on with a
   * caret pointing to the character that caused the error.
   *
   * Useful when using combinators to give meaningful error messages when things fail to parse correctly.
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
      def fold[B](folder: (Step[Array[Char], A]) => Future[B])(implicit ec: ExecutionContext) = {
        toMap.fold {
          // Handle the error before passing it to the folder
          case Step.Error(msg, remaining) => folder(handleError(state, msg, remaining))
          // More input? Map this ones errors too
          case Step.Cont(k) => folder(Step.Cont(in => mapErrors(state, k(in))))
          // Done? No errors to map.
          case s => folder(s)
        }(ec)
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
}
