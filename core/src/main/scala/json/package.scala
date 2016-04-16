package sfs

import scala.collection.JavaConverters._
import jdk.nashorn.api.scripting.{ ScriptObjectMirror => JsObject }
import javax.script.ScriptEngineManager

/*
 * Inefficient, but dependency-free json parser
 */
package object json {
  private val engine = new ScriptEngineManager(null).getEngineByName("nashorn")

  implicit class JsonForStrings(val s: String) extends AnyVal {

    def json = engine.eval(s"($s)") |> asScala

    private def asScala: Any => Any = {
      case o: JsObject if o.isArray => o.values.asScala.map(asScala)
      case o: JsObject              => o.asScala.toMap.mapValues(asScala)
      case o                        => o
    }
  }

  implicit class JsonOps(val o: Any) extends AnyVal {
    def as[T]: T                   = o.asInstanceOf[T]
    def asMap: Map[String, Object] = o.as[Map[String, Object]]
    def asSeq: Seq[Object]         = o.as[Seq[Object]]
  }
}
