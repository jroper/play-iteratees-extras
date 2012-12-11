package play.extras.iteratees

import play.api.libs.iteratee._

object CsvIteratee {
  def dropWhile(p: Char => Boolean): Iteratee[Char, Unit] = Cont {
    case in@Input.El(char) if !p(char) => Done(Unit, in)
    case in@Input.EOF => Done(Unit, in)
    case _ => dropWhile(p)
  }

  def dropSpaces = dropWhile(c =>
    c == ' ' || c == '\t' || c == '\r'
  )

  def takeWhile(p: Char => Boolean, data: Seq[Char] = IndexedSeq[Char]()): Iteratee[Char, Seq[Char]] = Cont {
    case in@Input.El(char) => if (p(char)) {
      takeWhile(p, data :+ char)
    } else {
      Done(data, in)
    }
    case in@Input.EOF => Done(data, in)
    case _ => takeWhile(p, data)
  }

  def peek: Iteratee[Char, Option[Char]] = Cont {
    case in@Input.El(char) => Done(Some(char), in)
    case in@Input.EOF => Done(None, in)
    case Input.Empty => peek
  }

  def takeOne: Iteratee[Char, Option[Char]] = Cont {
    case in@Input.El(char) => Done(Some(char))
    case in@Input.EOF => Done(None, in)
    case Input.Empty => takeOne
  }

  def expect(char: Char): Iteratee[Char, Unit] = takeOne.flatMap {
    case Some(c) if c == char => Done(Unit)
    case Some(c) => Error("Expected " + char + " but got " + c, Input.El(c))
    case None => Error("Premature end of input, expected: " + char, Input.EOF)
  }

  def unquoted = takeWhile(c => c != ',' && c != '\n').map(v => v.mkString.trim)

  def quoted(value: Seq[Char] = IndexedSeq[Char]()): Iteratee[Char, String] = for {
    _ <- expect('"')
    maybeValue <- takeWhile(_ != '"')
    _ <- expect('"')
    nextChar <- peek
    value <- nextChar match {
      case Some('"') => quoted(value ++ maybeValue :+ '"')
      case _ => Done[Char, String]((value ++ maybeValue).mkString)
    }
  } yield value

  def value = for {
    char <- peek
    value <- char match {
      case Some('"') => quoted()
      case None => Error[Char]("Premature end of input, expected a value", Input.EOF)
      case _ => unquoted
    }
  } yield value

  def values(state: Seq[String] = IndexedSeq[String]()): Iteratee[Char, Seq[String]] = for {
    _ <- dropSpaces
    value <- value
    _ <- dropSpaces
    nextChar <- takeOne
    values <- nextChar match {
      case Some('\n') | None => Done[Char, Seq[String]](state :+ value)
      case Some(',') => values(state :+ value)
      case Some(other) => Error("Expected comma, newline or EOF, but found " + other, Input.El(other))
    }
  } yield values

  def csvLines = Enumeratee.grouped(values())

}
