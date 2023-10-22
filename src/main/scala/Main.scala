// Simple server
import serve.Servable

import java.net.*
import java.io.*
import scala.io.*

object Main extends  App with Servable:
  listen(8080)
