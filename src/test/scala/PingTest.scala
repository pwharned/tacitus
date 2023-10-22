
import java.net._
import java.io._
import scala.io._
object PingTest extends App {



  val s = new Socket(InetAddress.getByName("localhost"), 8080)
  lazy val in = new BufferedSource(s.getInputStream()).getLines()
  val out = new PrintStream(s.getOutputStream())
  out.write("Hello, world".getBytes("utf-8"))


  val bytes = in
  println(bytes)

  //out.flush()
  //println("Received: " + in.next())
  s.close()

}
