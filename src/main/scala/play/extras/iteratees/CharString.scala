package play.extras.iteratees

/**
 * An efficient string of characters
 *
 * Not strictly immutable since it gives access to the underlying char array and doesn't copy char arrays when it
 * receives them, but none of the operations it provides are mutating.
 *
 * All of the operations provided here are provided just because they were needed by the Json parser, and for no
 * other reason.
 */
sealed trait CharString {
  /**
   * Make the CharString into a String
   */
  def mkString: String

  /**
   * The length of this CharString
   */
  def length: Int

  /**
   * Drop n chars from the CharString
   */
  def drop(n: Int): CharString

  /**
   * Drop while the given predicate is true
   */
  def dropWhile(p: Char => Boolean): CharString

  /**
   * Take n chars from the CharString
   */
  def take(n: Int): CharString

  /**
   * Take while the given predicate is true
   */
  def takeWhile(p: Char => Boolean): CharString

  /**
   * Concatenate the given CharString to this CharString
   */
  def ++(chars: CharString): CharString

  /**
   * Get the char item in the CharString if it exists
   */
  def headOption: Option[Char]

  /**
   * Get the count of chars that match the given predicate
   */
  def count(p: Char => Boolean): Int

  /**
   * Map this CharString using the given function
   */
  def map(f: Char => Char): CharString

  /**
   * Get the char at the given index
   */
  def charAt(i: Int): Char

  /**
   * Create a substring at the given offset of the given length
   */
  def substring(offset: Int, length: Int): CharString

  /**
   * Find the last index that matches the given predicate, starting from the given end position
   */
  def lastIndexWhere(p: Char => Boolean, end: Int = length - 1): Int
}

object CharString {

  /**
   * Create a CharString from the given char array
   */
  def fromCharArray(chars: Array[Char], offset: Int, length: Int): CharString = {
    if (length == 0) EmptyCharString
    else new ArrayCharString(chars, offset, length)
  }

  /**
   * Create a CharString from the given char array
   */
  def fromCharArray(chars: Array[Char]): CharString = fromCharArray(chars, 0, chars.length)

  /**
   * Create a CharString from the given String
   */
  def fromString(s: String): CharString = fromCharArray(s.toCharArray)

  /**
   * Create a CharString from the given char
   */
  def fromChar(char: Char): CharString = fromCharArray(Array[Char](char))

  /**
   * Create an empty CharString
   */
  def empty: CharString = EmptyCharString
}

private class ArrayCharString(val chars: Array[Char], val offset: Int, val length: Int) extends CharString {
  lazy val mkString = new String(chars, offset, length)

  def drop(n: Int) = {
    if (n >= length) EmptyCharString
    else new ArrayCharString(chars, offset + n, length - n)
  }

  def dropWhile(p: (Char) => Boolean) = {
    var toDrop = 0
    while (toDrop < length && p(chars(offset + toDrop))) {
      toDrop += 1
    }
    drop(toDrop)
  }

  def take(n: Int) = {
    if (n == 0) EmptyCharString
    else if (n >= length) this
    else new ArrayCharString(chars, offset, n)
  }

  def takeWhile(p: (Char) => Boolean) = {
    var toTake = 0
    while (toTake < length && p(chars(offset + toTake))) {
      toTake += 1
    }
    take(toTake)
  }

  def ++(other: CharString) = {
    other match {
      case EmptyCharString => this
      case acs: ArrayCharString => new JoinedCharString(List(acs, this))
      case jcs: JoinedCharString => new JoinedCharString(this :: jcs.charStrings)
    }
  }

  def headOption = {
    if (length > 0) {
      Some(chars(offset))
    } else {
      None
    }
  }

  def count(p: Char => Boolean) = {
    var counted = 0
    while (counted < length && p(chars(offset + counted))) {
      counted += 1
    }
    counted
  }

  def map(f: Char => Char) = {
    val charArray = new Array[Char](length)
    var i = 0
    while (i < length) {
      charArray(i) = f(chars(offset + i))
      i += 1
    }
    new ArrayCharString(charArray, 0, length)
  }

  def charAt(i: Int) = chars(i + offset)

  def substring(offset: Int, length: Int) = {
    if (length == 0) EmptyCharString
    else if (length > this.length) throw new ArrayIndexOutOfBoundsException(length)
    else new ArrayCharString(chars, this.offset + offset, length)
  }

  def lastIndexWhere(p: Char => Boolean, end: Int) = {
    chars.lastIndexWhere(p, end + offset) - offset
  }
}

private object EmptyCharString extends CharString {
  val length = 0
  val mkString = ""
  def drop(n: Int) = this
  def dropWhile(p: (Char) => Boolean) = this
  def take(n: Int) = this
  def takeWhile(p: (Char) => Boolean) = this
  def ++(chars: CharString) = chars
  def headOption = None
  def count(p: Char => Boolean) = 0
  def map(f: Char => Char) = this
  def charAt(i: Int) = throw new ArrayIndexOutOfBoundsException("Empty char string: " + i)
  def substring(o: Int, l: Int) = if (length == 0) this
    else throw new ArrayIndexOutOfBoundsException(length)
  def lastIndexWhere(p: Char => Boolean, end: Int) = 0
}

private class JoinedCharString(val charStrings: List[ArrayCharString]) extends CharString {
  def substring(offset: Int, length: Int) = {
    charStrings.foldRight((offset, length, CharString.empty)) {
      case (_, s @ (_, l, _)) if l <= 0 => s
      case (cs, (o, l, result)) if cs.length < o => (o - cs.length, l, result)
      case (cs, (o, l, result)) => (0, l - cs.length, result ++ cs.substring(o, l))
    }._3
  }

  def charAt(i: Int) = {
    charStrings.foldRight((i, '\0')) {
      case (_, result @ (index, c)) if index < 0 => result
      case (cs, (index, _)) if index < cs.length => (-1, cs.charAt(index))
      case (cs, (index, c)) => (index - cs.length, c)
    }._2
  }

  def map(f: (Char) => Char) = {
    new JoinedCharString(charStrings.map(_.map(f)))
  }

  def count(p: (Char) => Boolean) = {
    charStrings.foldLeft(0) { (c, cs) => cs.count(p) }
  }

  def headOption = charStrings.lastOption.flatMap(_.headOption)

  def ++(chars: CharString) = chars match {
    case EmptyCharString => this
    case acs: ArrayCharString => new JoinedCharString(acs :: charStrings)
    case jcs: JoinedCharString => new JoinedCharString(jcs.charStrings ++ charStrings)
  }

  def takeWhile(p: (Char) => Boolean) = {
    charStrings.foldRight[Either[CharString, CharString]](Left(EmptyCharString)) {
      case (_, result @ Right(_)) => result
      case (cs, Left(partial)) =>
        val taken = cs.takeWhile(p)
        if (taken.length == cs.length) {
          Left(partial ++ taken)
        } else {
          Right(partial ++ taken)
        }
    }.fold(identity, identity)
  }

  def take(n: Int) = {
    charStrings.foldRight[(Int, CharString)]((n, EmptyCharString)) {
      case (_, result @ (toTake, _)) if toTake <= 0 => result
      case (cs, (toTake, partial)) => (toTake - cs.length, partial ++ cs.take(toTake))
    }._2
  }

  def dropWhile(p: (Char) => Boolean) = {
    charStrings.foldRight[Option[CharString]](None) {
      case (cs, Some(keep)) => Some(keep ++ cs)
      case (cs, None) =>
        val dropped = cs.dropWhile(p)
        if (dropped.length == 0) {
          None
        } else {
          Some(dropped)
        }
    }.getOrElse(EmptyCharString)
  }

  def drop(n: Int) = {
    charStrings.foldRight[(Int, CharString)]((n, EmptyCharString)) {
      case (cs, (toDrop, keep)) if toDrop <= 0 => (toDrop, keep ++ cs)
      case (cs, (toDrop, _)) => (toDrop - cs.length, cs.drop(toDrop))
    }._2
  }

  lazy val length = {
    charStrings.foldLeft(0) { (l, cs) => cs.length + l }
  }

  lazy val mkString = {
    new String(compact)
  }

  def lastIndexWhere(p: Char => Boolean, end: Int) = {
    compact.lastIndexWhere(p, end)
  }

  private lazy val compact = {
    val arr = new Array[Char](length)
    charStrings.foldLeft(length) { (pos, cs) =>
      val p = pos - cs.length
      System.arraycopy(cs.chars, cs.offset, arr, p, cs.length)
      p
    }
    arr
  }
}