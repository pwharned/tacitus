package serve

import java.io.PrintStream
import java.net.ServerSocket
import scala.io.BufferedSource
import scala.language.postfixOps
import scala.util.Try

trait Servable:

  def listen(port:Int): Unit =
    val server = new ServerSocket(port)

    while true
        do
        val s = server.accept()
        val in = new BufferedSource(s.getInputStream()).getLines()
        val out = new PrintStream(s.getOutputStream())

        while Try(in.hasNext).getOrElse(false)
          do
          println(in.next())

          val chunked = """Transfer-Encoding: chunked\r\n
                          |Content-Type: text/plain\r\n
                          |\r\n
                          |4\r\n
                          |Wiki\r\n
                          |5;extkey=extvalue\r\n
                          |pedia\r\n
                          |E\r\n
                          |.in\r\n
                          |\r\n
                          |chunks.\r\n
                          |0\r\n
                          |HTTP/1.1 200 OK\r\n
                          |\r\n""".stripMargin

          val bytes = """HTTP/1.1 200 OK
            |Server: Tacitus/0.1
            |Date: Wed, 18 Oct 2017
            |Content-type: text/html
            |Content-Length: 23
            |
            |""".stripMargin
          val body = "Hello, world!"
          out.write(bytes.getBytes("utf-8") )
          out.write(body.getBytes("utf-8") )

        out.flush()
        s.close()

