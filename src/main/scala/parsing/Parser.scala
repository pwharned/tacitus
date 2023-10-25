import scala.util.{Failure, Success, Try}

trait Parsers[ParseError, Parser[+_]]:
 // def char(c:Char): Parser[Char] = string(c.toString).map( x=> x.charAt(0))
  def string(s: String): Parser[String]
  /*
    def succeed[A](a:A): Parser[A] = string("").map(x => x)
  */
    extension[A](p:Parser[A])
      infix def or(p2: => Parser[A]): Parser[A]
      def |(p2: => Parser[A]): Parser[A] = p.or(p2)
      /*
      def map[B](f: A => B): Parser[B]
      def run(input:String): Either[ParseError, A]


      def listOfN(n: Int): Parser[List[A]]

      def many: Parser[List[A]]

      infix def product[B]( p2: Parser[B]): Parser[(A, B)]
      def **[B]( p2: Parser[B]): Parser[(A, B)] = product(p2)
      */

end Parsers



object Reference extends Parsers[Exception, Reference.Parser]:


  type Parser[+A] = String => A


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


@main def run(): Unit = {



    def jsonParser[Parser[+_]](P: Parsers[Exception, Parser]): Parser[String] =
      import P.*
      string("hello") or string("goodbye")


}