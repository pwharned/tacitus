package serve.http
import java.net.Socket
import scala.io.BufferedSource
import scala.language.postfixOps


object HTTPRequest:

  class HTTPRequest(s: Socket) extends Socket with  Iterator[String]:

    lazy val i = new BufferedSource(s.getInputStream()).getLines()
    override def hasNext: Boolean = i.hasNext


    override def next(): String = {
      if !i.isEmpty then i.next() else ""
    }

    def headers: Iterator[String] = i.filter(x => x!="")

  given fromSocketToRequest: Conversion[Socket, HTTPRequest] = (s: Socket) => HTTPRequest(s)




