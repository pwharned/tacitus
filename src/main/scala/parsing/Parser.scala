package parsing


import java.util.regex.*
import scala.util.matching.Regex

trait Parsers[Parser[+_]]:

  def string(s: String): Parser[String]

  def char(c: Char): Parser[Char] =
    string(c.toString).map(_.charAt(0))

  /*
   * A default `succeed` implementation in terms of `string` and `map`.
   * We leave `succeed` abstract, since `map` is defined below in terms of
   * `flatMap` and `succeed`, which would be a circular definition! But we include
   * the definition here in case implementations wish to use it
   * (say if they provide a custom implementation of `map`, breaking the cycle)
   */
  def defaultSucceed[A](a: A): Parser[A] =
    string("").map(_ => a)

  def succeed[A](a: A): Parser[A]

  def fail(msg: String): Parser[Nothing]

  def regex(r: Regex): Parser[String]

  /** Parser which consumes zero or more whitespace characters. */
  def whitespace: Parser[String] = regex("\\s*".r)

  /** Parser which consumes 1 or more digits. */
  def digits: Parser[String] = regex("\\d+".r)

  /** Parser which consumes reluctantly until it encounters the given string. */
  def thru(s: String): Parser[String] = regex((".*?" + Pattern.quote(s)).r)

  /** Unescaped string literals, like "foo" or "bar". */
  def quoted: Parser[String] = string("\"") *> thru("\"").map(_.dropRight(1))

  /** Unescaped or escaped string literals, like "An \n important \"Quotation\"" or "bar". */
  def escapedQuoted: Parser[String] =
  // rather annoying to write, left as an exercise
  // we'll just use quoted (unescaped literals) for now
    quoted.label("string literal").token

  /** C/Java style floating point literals, e.g .1, -1.0, 1e9, 1E-23, etc.
   * Result is left as a string to keep full precision
   */
  def doubleString: Parser[String] =
    regex("[-+]?([0-9]*\\.)?[0-9]+([eE][-+]?[0-9]+)?".r).token

  /** Floating point literals, converted to a `Double`. */
  def double: Parser[Double] =
    doubleString.map(_.toDouble).label("double literal")

  /** A parser that succeeds when given empty input. */
  def eof: Parser[String] =
    regex("\\z".r).label("unexpected trailing characters")

  extension [A](p: Parser[A])

    def run(input: String): Either[ParseError, A]

    infix def or(p2: => Parser[A]): Parser[A]
    def |(p2: => Parser[A]): Parser[A] = p.or(p2)

    def attempt: Parser[A]

    def listOfN(n: Int): Parser[List[A]] =
      if n <= 0 then succeed(Nil)
      else p.map2(p.listOfN(n - 1))(_ :: _)

    def map[B](f: A => B): Parser[B] =
      p.flatMap(f andThen succeed)

    def map2[B, C](p2: => Parser[B])(f: (A, B) => C): Parser[C] =
      p.product(p2).map((a, b) => f(a, b))

    def many: Parser[List[A]] =
      p.map2(p.many)(_ :: _) | succeed(Nil)

    def many1: Parser[List[A]] =
      p.map2(p.many)(_ :: _)

    def slice: Parser[String]

    def opt: Parser[Option[A]] =
      p.map(Some(_)) | succeed(None)

    def product[B](p2: => Parser[B]): Parser[(A, B)] =
      p.flatMap(a => p2.map(b => (a, b)))

    def **[B](p2: => Parser[B]): Parser[(A,B)] = product(p2)

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def label(msg: String): Parser[A]

    def scope(msg: String): Parser[A]

    /** Sequences two parsers, ignoring the result of the first.
     * We wrap the ignored half in slice, since we don't care about its result.
     */
    def *>[B](p2: => Parser[B]) =
      p.slice.map2(p2)((_, b) => b)

    /** Sequences two parsers, ignoring the result of the second.
     * We wrap the ignored half in slice, since we don't care about its result.
     */
    def <*(p2: => Parser[Any]) =
      p.map2(p2.slice)((a, b) => a)

    /** Attempts `p` and strips trailing whitespace, usually used for the tokens of a grammar. */
    def token: Parser[A] = p.attempt <* whitespace

    /** Zero or more repetitions of `p`, separated by `p2`, whose results are ignored. */
    def sep(separator: Parser[Any]): Parser[List[A]] = // use `Parser[Any]` since don't care about result type of separator
      p.sep1(separator) | succeed(Nil)

    /** One or more repetitions of `p`, separated by `p2`, whose results are ignored. */
    def sep1(separator: Parser[Any]): Parser[List[A]] =
      p.map2((separator *> p).many)(_ :: _)

    def as[B](b: B): Parser[B] = p.slice.map(_ => b)

    /** Parses a sequence of left-associative binary operators with the same precedence. */
    def opL(op: Parser[(A, A) => A]): Parser[A] =
      p.map2((op ** p).many)((h, t) => t.foldLeft(h)((a, b) => b._1(a, b._2)))

    /** The root of the grammar, expects no further input following `p`. */
    def root: Parser[A] =
      p <* eof



end Parsers

case class Location(input: String, offset: Int = 0):

  lazy val line = input.slice(0, offset + 1).count(_ == '\n') + 1

  lazy val col = input.slice(0, offset + 1).lastIndexOf('\n') match
    case -1 => offset + 1
    case lineStart => offset - lineStart

  def toError(msg: String): ParseError =
    ParseError(List((this, msg)))

  def advanceBy(n: Int) = copy(offset = offset + n)

  def remaining: String = input.substring(offset)

  def slice(n: Int) = input.substring(offset, offset + n)

  /* Returns the line corresponding to this location */
  def currentLine: String =
    if input.length > 1
    then
      val itr = input.linesIterator.drop(line - 1)
      if (itr.hasNext) itr.next() else ""
    else ""

  def columnCaret = (" " * (col - 1)) + "^"

case class ParseError(stack: List[(Location, String)] = Nil):
  def push(loc: Location, msg: String): ParseError =
    copy(stack = (loc, msg) :: stack)

  def label(s: String): ParseError =
    ParseError(latestLoc.map((_, s)).toList)

  def latest: Option[(Location,String)] =
    stack.lastOption

  def latestLoc: Option[Location] =
    latest map (_._1)

  /**
  Display collapsed error stack - any adjacent stack elements with the
  same location are combined on one line. For the bottommost error, we
  display the full line, with a caret pointing to the column of the error.
  Example:

  1.1 file 'companies.json'; array
  5.1 object
  5.2 key-value
  5.10 ':'

  { "MSFT" ; 24,
           ^
   */
  override def toString =
    if stack.isEmpty then "no error message"
    else
      val collapsed = collapseStack(stack)
      val context =
        collapsed.lastOption.map("\n\n" + _._1.currentLine).getOrElse("") +
          collapsed.lastOption.map("\n" + _._1.columnCaret).getOrElse("")
      collapsed.map((loc, msg) => s"${formatLoc(loc)} $msg").mkString("\n") + context

  /* Builds a collapsed version of the given error stack -
   * messages at the same location have their messages merged,
   * separated by semicolons */
  def collapseStack(s: List[(Location, String)]): List[(Location, String)] =
    s.groupBy(_._1).
      view.
      mapValues(_.map(_._2).mkString("; ")).
      toList.sortBy(_._1.offset)

  def formatLoc(l: Location): String = s"${l.line}.${l.col}"





object Reference extends Parsers[Reference.Parser]:

  /** A parser is a kind of state action that can fail. */
  // https://github.com/lampepfl/dotty/issues/13761
  type Parser[+A] = Location => Result[A]

  enum Result[+A]:
    case Success(get: A, length: Int)
    case Failure(get: ParseError, isCommitted: Boolean) extends Result[Nothing]

    def extract: Either[ParseError, A] = this match
      case Failure(e,_) => Left(e)
      case Success(a,_) => Right(a)

    /* Used by `attempt`. */
    def uncommit: Result[A] = this match
      case Failure(e, true) => Failure(e, false)
      case _ => this

    /* Used by `flatMap`. */
    def addCommit(isCommitted: Boolean): Result[A] = this match
      case Failure(e, c) => Failure(e, c || isCommitted)
      case _ => this

    /* Used by `scope`, `label`. */
    def mapError(f: ParseError => ParseError): Result[A] = this match
      case Failure(e, c) => Failure(f(e), c)
      case _ => this

    def advanceSuccess(n: Int): Result[A] = this match
      case Success(a,m) => Success(a,n+m)
      case _ => this

  // consume no characters and succeed with the given value
  def succeed[A](a: A): Parser[A] =
    _ => Result.Success (a, 0)

  /** Returns -1 if s1.startsWith(s2), otherwise returns the
   * first index where the two strings differed. If s2 is
   * longer than s1, returns s1.length. */
  def firstNonmatchingIndex(s1: String, s2: String, offset: Int): Int =
    var i = 0
    while i + offset < s1.length && i < s2.length do
      if s1.charAt(i + offset) != s2.charAt(i) then return i
      i += 1
    if s1.length - offset >= s2.length then -1
    else s1.length - offset

  def string(w: String): Parser[String] =
    l =>
      val i = firstNonmatchingIndex(l.input, w, l.offset)
      if i == -1 then // they matched
        Result.Success(w, w.length)
      else
        Result.Failure(l.advanceBy(i).toError(s"'$w'"), i != 0)

  /* note, regex matching is 'all-or-nothing':
   * failures are uncommitted */
  def regex(r: Regex): Parser[String] =
    l => r.findPrefixOf(l.remaining) match
      case None => Result.Failure(l.toError(s"regex $r"), false)
      case Some(m) => Result.Success(m, m.length)

  def fail(msg: String): Parser[Nothing] =
    l => Result.Failure(l.toError(msg), true)

  extension [A](p: Parser[A])

    def run(s: String): Either[ParseError, A] =
      p(Location(s)).extract

    def or(p2: => Parser[A]): Parser[A] =
      l => p(l) match
        case Result.Failure(e, false) => p2(l)
        case r => r // committed failure or success skips running `p2`

    def attempt: Parser[A] = l => p(l).uncommit

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      l => p(l) match
        case Result.Success(a, n) =>
          f(a)(l.advanceBy(n))
            .addCommit(n != 0)
            .advanceSuccess(n)
        case f @ Result.Failure(_, _) => f

    def slice: Parser[String] =
      l => p(l) match
        case Result.Success(_, n) => Result.Success(l.slice(n), n)
        case f @ Result.Failure(_, _) => f

    /* We provide an overridden version of `many` that accumulates
    * the list of results using a monolithic loop. This avoids
    * stack overflow errors for most grammars.
    */
    override def many: Parser[List[A]] =
      l =>
        var nConsumed: Int = 0
        val buf = new collection.mutable.ListBuffer[A]
        def go(p: Parser[A], offset: Int): Result[List[A]] =
          p(l.advanceBy(offset)) match
            case Result.Success(a, n) =>
              buf += a
              go(p, offset + n)
            case Result.Failure(e, true) => Result.Failure(e, true)
            case Result.Failure(_, _) => Result.Success(buf.toList, offset)
        go(p, 0)

    def scope(msg: String): Parser[A] =
      l => p(l).mapError(_.push(l, msg))

    def label(msg: String): Parser[A] =
      l => p(l).mapError(_.label(msg))


enum JSON:
  case JNull
  case JNumber(get: Double)
  case JString(get: String)
  case JBool(get: Boolean)
  case JArray(get: IndexedSeq[JSON])
  case JObject(get: Map[String, JSON])

object JSON:
  def jsonParser[Parser[+_]](P: Parsers[Parser]): Parser[JSON] =
    import P.*

    def token(s: String) = string(s).token

    def array: Parser[JSON] = (
      token("[") *> value.sep(token(",")).map(vs => JArray(vs.toIndexedSeq)) <* token("]")
      ).scope("array")

    def obj: Parser[JSON] = (
      token("{") *> keyval.sep(token(",")).map(kvs => JObject(kvs.toMap)) <* token("}")
      ).scope("object")

    def keyval: Parser[(String, JSON)] = escapedQuoted ** (token(":") *> value)

    def lit: Parser[JSON] = (
      token("null").as(JNull) |
        double.map(JNumber(_)) |
        escapedQuoted.map(JString(_)) |
        token("true").as(JBool(true)) |
        token("false").as(JBool(false))
      ).scope("literal")

    def value: Parser[JSON] = lit | obj | array

    (whitespace *> (obj | array)).root

/**
 * JSON parsing example.
 */
@main def jsonExample =
  val jsonTxt = """
{
  "Company name" : "Microsoft Corporation",
  "Ticker"  : "MSFT",
  "Active"  : true,
  "Price"   : 30.66,
  "Shares outstanding" : 8.38e9,
  "Related companies" : [ "HPQ", "IBM", "YHOO", "DELL", "GOOG" ]
}
"""

  val malformedJson1 = """
{
  "Company name" ; "Microsoft Corporation"
}
"""

  val malformedJson2 = """
[
  [ "HPQ", "IBM",
  "YHOO", "DELL" ++
  "GOOG"
  ]
]
"""

  def printResult[E](e: Either[E,JSON]) =
    e.fold(println, println)

  val parser = JSON.jsonParser(Reference)
  printResult(parser.run(jsonTxt))
  println("--")
  printResult(parser.run(malformedJson1))
  println("--")
  printResult(parser.run(malformedJson2))