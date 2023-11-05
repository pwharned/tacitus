// Simple server
import serve.Servable


object Main extends  App with Servable:
  listen(8080)
