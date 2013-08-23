package play.extras.iteratees

import java.nio.charset.{Charset, CharsetDecoder}
import play.api.libs.iteratee._
import play.api.libs.iteratee.Input.{El, Empty, EOF}
import java.nio.{ByteBuffer, CharBuffer}
import play.api.libs.concurrent.Execution.Implicits._

/**
 * Enumeratees for dealing with character encoding
 */
object Encoding {

  /**
   * Converts a stream of byte arrays to a stream of char arrays, taking into account the possibility of split multi
   * byte characters.
   *
   * @param decoder The decoder to use.  Defaults to a UTF-8 decoder
   */
  def decode(decoder: CharsetDecoder = Charset.forName("UTF-8").newDecoder()): Enumeratee[Array[Byte], Array[Char]] = new Enumeratee[Array[Byte],Array[Char]] {
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
