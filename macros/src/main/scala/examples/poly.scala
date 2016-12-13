package examples

import scala.meta._
import scala.collection.immutable.Seq
class poly extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case q"def $name[$param]($in: $inT[$p1]): $outT[..$outParams] = $impl" =>
        println(inT)
        val valName = Pat.Var.Term(Term.Name(name.value))

        val newType = Type.fresh("poly")

        val termName = Term.Name(newType.value)


        val params = outParams.take(outParams.length - 1)

        q"""
            val $valName: _root_.cats.arrow.FunctionK[$inT, ({ type T[poly1] = $outT[..${params ++ Seq(newType)}]})#T] =
              new _root_.cats.arrow.FunctionK[$inT, ({ type T[poly1] = $outT[..${params ++ Seq(newType)}]})#T] {
                def apply[$param]($in: $inT[$p1]): $outT[..$outParams] = $impl
              }
          """
    }
  }
}