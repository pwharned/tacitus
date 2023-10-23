package json
import scala.language.implicitConversions
import scala.util.{Failure, Success, Try}

sealed trait Json

object Json extends Json {

  val comma = ",(?![^\\{|\\[]*[\\}|\\]^])"

  //val colin = ":(?![^\\{]*[\\}^])"

  val colin = ":(?!\\w*\")(?![^\\{]*[\\}^])(?<!\\w*\")"
  //https://stackoverflow.com/questions/1443360/regex-for-matching-a-character-but-not-when-its-enclosed-in-quotes

  case class JsonString( value: String) extends Json {
    override def toString: String =  "\"" +  value.replaceAll("\"", "") + "\""
  }


  sealed trait JsonNumber[T]

  case class JsonInt(value: Int) extends Json with JsonNumber[Int]{
    override def toString: String = value.toString
  }

  case class JsonAny(value: Any) extends Json {

    override def toString: String = value match {
      case value: Int => JsonInt(value).toString
      case value: String => {JsonString(value).toString}
      case value: Seq[Any] => JsonList(value.map(x => JsonAny(x)):_*).toString
      case _  => value.toString
    }
  }

  case class JsonDouble(value: Double) extends Json with  JsonNumber[Double]{
    override def toString: String = value.toString
  }



  case class JsonProduct(value: Product) extends Json {


   override def toString: String = JsonMap(
      value.productElementNames.zip(value.productIterator.toList.flatMap { v =>
        v match {

          case x: List[Any] => x.map(x => Json.JsonAny(x))
          case x: Map[String, Any] => List(Json.JsonMap(x.map(x => (x._1, Json.JsonAny(x._2)))))

          case x: List[Map[String, Any]] => x.map(x => Json.JsonMap(x.map(x => (x._1, Json.JsonAny(x._2)))))
          case x: Any => List(x).map(x => Json.JsonAny(x))

        }

      }).toMap

    ).toString

  }


  case class JsonBoolean(value: Boolean) extends Json{
    override def toString: String = value.toString
  }
  case class JsonNull(value: Null) extends Json{
    override def toString: String = value
  }
  case class JsonList(items: Json*) extends Json {
    override def toString: String = "[" + items.mkString(",") + "]"
  }

  case class JsonMap(val items: Map[String,Json]) extends Json{

    override def toString: String = "{" +  items.map{ case (k->v) => "\""+k +"\"" + ":" + v.toString   }.mkString(",") + "}"

    def toMap: Map[String, String] = items.map{
      case (k -> v) => k -> v.toString.replaceAll("\"", "")
    }

    def get(key: String): Option[Json] = items.get(key)
  }

  object JsonMap {
    def fromString(x: String): JsonMap = {
      val map = x.trim.stripPrefix("{").stripSuffix("}").trim.split(Json.comma).map(_.split(Json.colin)).map {

        case Array(k, v) => (k.replaceAll("\"", "").trim, Json.fromString(v.replaceAll("\"", "").trim))
          

      }.toMap
      JsonMap(map)
    }


  }

  object JsonList {

    def fromString(x: String): JsonList = {
      val seq = x.trim.stripPrefix("[").stripSuffix("]").trim.split(Json.comma).toSeq.map(x => Json.fromString(x))
      JsonList(seq:_*)

    }

  }

  def apply[T](x:T)(implicit converter: JsonValue[T]): Json = converter.serialize(x)

  def fromString(y: String)  = {
    val x = y.trim()
    //println(x)
    x match {
      case x if x.startsWith("[") => {   JsonList.fromString(x)}
      case x if x.startsWith("{") => {  JsonMap.fromString(x) }
      case "null" =>  JsonNull(value = null)
      case "True" =>  JsonBoolean(true)
      case "False"=>  JsonBoolean(false)
      case x if x.matches("[0-9]+") => JsonInt(value = x.toInt)
      case x if Try{x.toDouble}.isSuccess =>  JsonDouble(value = x.toDouble  )
      case x: String => JsonString(x.replaceAll("\"", ""))
    }}



}

trait JsonValue[T] {

  def serialize(t: T): Json

}

object JsonValue{
  implicit object StringJsonValue extends JsonValue[String]{
    def serialize(t: String): Json.JsonString = Json.JsonString(t)
  }
  implicit object DoubleJsonValue extends JsonValue[Double]{
    def serialize(t: Double): Json.JsonDouble = Json.JsonDouble(t)
  }
  implicit object IntJsonValue extends JsonValue[Int]{
    def serialize(t: Int): Json.JsonInt = Json.JsonInt(t)
  }
  implicit object BooleanJsonValue extends JsonValue[Boolean]{
    def serialize(t: Boolean): Json.JsonBoolean = Json.JsonBoolean(t)
  }
  implicit object NullJsonValue extends JsonValue[Null]{
    def serialize(t: Null): Json.JsonNull = Json.JsonNull(t)
  }
  implicit object AnyJsonValue extends JsonValue[Any]{
    def serialize(t: Any): Json.JsonAny = Json.JsonAny(t)
  }
  implicit object ProductJsonValue extends JsonValue[Product]{
    def serialize(t: Product): Json.JsonProduct = Json.JsonProduct(t)
  }
  implicit def MapJsonValue[T: JsonValue]: JsonValue[Map[String, T]] = (t: Map[String, T]) => {
    Json.JsonMap(t.map(t => (t._1, implicitly[JsonValue[T]].serialize(t._2))))
  }
  implicit def SeqJsonValue[T: JsonValue]: JsonValue[Seq[T]] = (t: Seq[T]) => {
    Json.JsonList(t.map(implicitly[JsonValue[T]].serialize): _*)
  }
}





object test extends App {


  var jsonMap = Json.fromString(
    """
      {
       "username": "vly34104",
        "port": "31321",
        "host": "ba99a9e6-d59e-4883-8fc0-d6a8c9f7a08f.c1ogj3sd0tgtu0lqde00.databases.appdomain.cloud:10",
        "password": "7LKhm8kHnNOegkZa",
        "database":  "bludb",
        "driver":.jcc4
      }"""
  )

  println(jsonMap)




  private case class Person(name: String)

  val myperson: Product= Person("john")
  val personSeq: Seq[Product] = Seq(myperson)

  implicit def toJson(p: Product): String = Json(p).toString

  toJson(myperson)


  println(Json(myperson))

  print(Json(personSeq))
}