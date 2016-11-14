package examples
import scala.meta._
import scala.annotation.compileTimeOnly

@compileTimeOnly("@examples.Value not expanded")
class value extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"..$mods class $name" =>
        q"final case class $name(value: String) extends AnyVal"
    }
  }
}
