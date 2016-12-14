package examples


import scala.collection.immutable.Seq
import scala.meta._

class free extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Term.Block(Seq(t: Defn.Trait, companion: Defn.Object)) =>
        FreeMacro(t, Some(companion))
      case t: Defn.Trait =>
        FreeMacro(t, None)
    }
  }
}

object FreeMacro {
  def apply(t: Defn.Trait, companion: Option[Defn.Object]): Term.Block = {
    println(s"IN: \n $t ${companion.map(_.toString).getOrElse("")}")
    val freeName = s"${t.name.value}Free"

    val traitStats = t.templ.stats.get

    val cases = traitStats.map {
      case q"def $name[..$tps](..$params): F[$out]" =>
        q"final case class ${Type.Name(name.value.capitalize)}[..$tps](..$params) extends ${Ctor.Name(freeName)}[$out]"
    }

    def toCaseName(name: Term.Name) = s"$freeName.${name.value.capitalize}"
    val freeTypeName = Type.Name(freeName)

    val methods = traitStats.map {
      case q"def $name[..$tps](..$params): F[$out]" =>
        val ctor = Ctor.Name(toCaseName(name))
        val args = params.map(_.name.value).map(Term.Name(_))
        q"def $name[..$tps](..$params): F[$out] = f($ctor(..$args))"
    }

    val patMatCases = traitStats.map {
      case q"def $methodName[..$tps](..$params): F[$out]" =>
        val args = params.map(_.name.value).map(Term.Name(_))
        val exractArgs = args.map(Pat.Var.Term(_))
        val caseName = Term.Name(toCaseName(methodName))
        p"case $caseName(..$exractArgs) => ops.$methodName(..$args)"
    }

    val helperName = Type.fresh("FreeHelper")

    val companionStats: Seq[Stat] = Seq(
      q"def apply[F[_]](implicit instance: ${t.name}[F]): ${t.name}[F] = instance",
      q"sealed abstract class $freeTypeName[A] extends Product with Serializable",
      q"""object ${Term.Name(freeName)} {
        ..$cases
      }
      """
      ,q"""def fromFunctionK[F[_]](f: _root_.cats.arrow.FunctionK[$freeTypeName, F]): ${t.name}[F] =
         new ${Ctor.Name(t.name.value)}[F] {
          ..$methods
          }
       """
    ,
      q"""def toFunctionK[F[_]](ops: ${t.name}[F]): _root_.cats.arrow.FunctionK[$freeTypeName, F] =
         new _root_.cats.arrow.FunctionK[$freeTypeName, F] {
          def apply[A](op: $freeTypeName[A]): F[A] =
            op match { ..case $patMatCases }
         }
       """
      ,
        q"""
           private sealed trait $helperName[F[_]] {
                      type Out[A] = Free[F, A]
           }
         """
    ,
    q"""
       implicit def free[F[_]](implicit inject: _root_.cats.free.Inject[$freeTypeName, F]): KeyValueStore[$helperName[F]#Out]  = {
         val f = new _root_.cats.arrow.FunctionK[$freeTypeName, $helperName[F]#Out] {
           def apply[A](op: $freeTypeName[A]): Free[F, A] = _root_.cats.free.Free.inject(op)
         }
         fromFunctionK(f)
       }
     """)


    val newCompanion = companion match {
      case Some(c) =>
        val oldTemplStats = c.templ.stats.getOrElse(Nil)
          c.copy(templ = c.templ.copy(stats = Some(companionStats ++ oldTemplStats)))
      case None =>
        q"object ${Term.Name(t.name.value)} { ..$companionStats }"

    }

    val result = Term.Block(Seq(t, newCompanion))
    println(s"OUT: \n $result")
    result
  }
}


