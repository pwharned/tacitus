package serve

import java.io.PrintStream
import java.net.ServerSocket
import scala.io.BufferedSource
import scala.language.postfixOps
import scala.util.Try
import serve.http.HTTPRequest.{HTTPRequest, fromSocketToRequest}
trait Servable:

  def listen(port:Int): Unit =
    val server = new ServerSocket(port)

    while true
    do
      val s = server.accept()
      val req: HTTPRequest = s
      val out = new PrintStream(s.getOutputStream())

      while Try(req.hasNext).getOrElse(false)
      do

        req.headers.foreach(println)

        println(req.next())

        val chunked = """HTTP/1.1 200 OK
                          |Content-Type: text/plain
                          |Transfer-Encoding: chunked
                          |
                          |7
                          |Mozilla
                          |
                          |7
                          |Mozilla
                          |
                          |0""".stripMargin


        //out.write(bytes.getBytes("utf-8") )
        out.write(chunked.getBytes("utf-8") )


        out.flush()
        out.close()


