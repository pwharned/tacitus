
import scala.util.matching.Regex

trait Parsers[Parser[+_]]:
  def regex(r: Regex): Parser[String]

  def succeed[A](a: A): Parser[A]

  def fail(msg: String): Parser[Nothing]

  extension [A](p: Parser[A])

    def flatMap[B](f: A => Parser[B]): Parser[B]

    def map[B](f: A => B): Parser[B] =
      p.flatMap(f andThen succeed)

end Parsers



case class Location(input: String, offset: Int = 0):
  def advanceBy(n: Int) = copy(offset = offset + n)

  def remaining: String = input.substring(offset)


object Reference extends Parsers[Reference.Parser]:

/** A parser is a kind of state action that can fail. */
// https://github.com/lampepfl/dotty/issues/13761
  type Parser[+A] = Location => Result[A]

  def succeed[A](a: A): Parser[A] =
    _ => Result.Success(a, 0)

  def fail(msg: String): Parser[Nothing] =
      l => Result.Failure(new Exception(msg), true)


  def regex(r: Regex): Parser[String] =
  l => r.findPrefixOf(l.remaining) match
    case None => Result.Failure(new Exception(), false)
    case Some(m) => Result.Success(m, m.length)

  extension [A](p: Parser[A])
    def flatMap[B](f: A => Parser[B]): Parser[B] =
      l => p(l) match
        case Result.Success(a, n) =>
          f(a)(l.advanceBy(n))
        case f@Result.Failure(_, _) => f

    def run(s: String): Either[Exception, A] =
        p(Location(s)).extract


  enum Result[+A]:
      case Success(get: A, length: Int)
      case Failure(get: Exception, isCommitted: Boolean) extends Result[Nothing]

      def extract: Either[Exception, A] = this match
        case Failure(e,_) => Left(e)
        case Success(a,_) => Right(a)
class Examples[Parser[+_]](P: Parsers[Parser]):
  import P.*

  val nonNegativeInt: Parser[Int] =
    for
      nString <- regex("[0-9]+".r)
      n <- nString.toIntOption match
        case Some(n) => succeed(n)
        case None => fail("expected an integer")
    yield n

object test extends App{
  val test = new Examples(Reference)

test.nonNegativeInt.run("1,2") match{
    case Left(value) => value.printStackTrace()
    case Right(value) => println(value)
  }

}