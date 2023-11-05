import scala.util.{Failure, Success, Try}

trait Parsers[E, P[+_]]:
 // def char(c:Char): Parser[Char] = string(c.toString).map( x=> x.charAt(0))
  def string(s: String): P[String]

  def succeed[A](a: A): P[A]
  /*
    def succeed[A](a:A): Parser[A] = string("").map(x => x)
  */
    extension[A](p:P[A])
      infix def or(p2: => P[A]): P[A]
      def |(p2: => P[A]): P[A] = p.or(p2)
      /*
      def listOfN(n: Int): P[List[A]]
      def many: P[List[A]]
      */
      def map[B](f: A => B): P[B] =
        p.flatMap(f andThen succeed)
      def flatMap[B](f: A => P[B]): P[B]

  /*
  def map[B](f: A => B): Parser[B]
  def run(input:String): Either[ParseError, A]



  def many: Parser[List[A]]

  infix def product[B]( p2: Parser[B]): Parser[(A, B)]
  def **[B]( p2: Parser[B]): Parser[(A, B)] = product(p2)
  */

end Parsers



object Reference extends Parsers[Exception, Reference.Parser]:


  type Parser[+A] = String => A

  def succeed[A](a: A): Parser[A] =
    _ => a



  def firstNonmatchingIndex(s1: String, s2: String, offset: Int=0): Int =
    var i = 0
    while i + offset < s1.length && i < s2.length do
      if s1.charAt(i + offset) != s2.charAt(i) then return i
      i += 1
    if s1.length - offset >= s2.length then -1
    else s1.length - offset

  def string(w: String): Parser[String] =
    l =>
      val i = firstNonmatchingIndex(l, w)
      if i == -1 then // they matched
        w
      else
        throw new Exception("Parsing failure")

  extension [A](p: Parser[A])
    def or(p2: => Parser[A]): Parser[A] = l => Try(p(l)) match
      case Success(value) => value
      case Failure(exception) => p2(l)

    def flatMap[B](f: A => Parser[B]): Parser[B] =
      l => f(p(l))("hel")


@main def run(): Unit = {



    def jsonParser[Parser[+_]](P: Parsers[Exception, Parser]): Parser[String] =
      import P.*
      string("hello") or string("goodbye")

  assert(jsonParser(Reference).apply("hello") == "hello")
  assert(jsonParser(Reference).apply("goodbye") == "goodbye")


}