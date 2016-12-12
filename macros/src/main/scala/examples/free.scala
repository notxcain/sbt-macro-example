package examples


import scala.collection.immutable.Seq
import scala.meta._

class free extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Term.Block(Seq(t @ ClassOrTraitWithOneTypeParameter(mods, name), companion: Defn.Object))
        if FreeMacro.isSealed(mods) =>

        val oldTemplStats = companion.templ.stats.getOrElse(Nil)
        val subTypes = oldTemplStats.collect {
          case t: Defn.Class if FreeMacro.inherits(name)(t) => t
        }

        val newStats =
          FreeMacro.mkTraitF(name, subTypes) +: oldTemplStats

        val newCompanion =
          companion.copy(templ = companion.templ.copy(stats = Some(newStats)))

        Term.Block(Seq(t, newCompanion))
    }
  }
}

class genfree extends scala.annotation.StaticAnnotation {
  inline def apply(defn: Any): Any = meta {
    defn match {
      case Term.Block(Seq(t: Defn.Trait, companion: Defn.Object)) =>
        FreeGenMacro.gen(t, Some(companion))
      case t: Defn.Trait =>
        FreeGenMacro.gen(t, None)
    }
  }
}

object FreeGenMacro {
  def gen(t: Defn.Trait, companion: Option[Defn.Object]): Term.Block = {
    println(s"IN: $t $companion")
    val opsName = s"${t.name.value}Free"

    val stats = t.templ.stats.get.map {
      case q"def $name[..$tps]($params): F[$out]" =>
        q"final case class ${Type.Name(name.value.capitalize)}[..$tps]($params) extends ${Ctor.Name(opsName)}[$out]"
    }

    val out: Seq[Stat] = Seq(
      q"""sealed abstract class ${Type.Name(opsName)}[A] extends Product with Serializable""",
          q"""object ${Term.Name(opsName)} {
            ..$stats
          }
          """)


    val newCompanion = companion match {
      case Some(c) =>
        val oldTemplStats = c.templ.stats.getOrElse(Nil)
          c.copy(templ = c.templ.copy(stats = Some(out ++ oldTemplStats)))
      case None =>
        q"object ${Term.Name(t.name.value)} { ..$out }": Defn.Object

    }

    val result = Term.Block(Seq(t, newCompanion))
    println(s"OUT: $result")
    result
  }
}

object FreeMacro {

  def isSealed(mods: Seq[Mod]): Boolean = mods.exists(_.syntax == "sealed")

  def inherits(superType: Type.Name)(cls: Defn.Class): Boolean =
    cls.templ.parents.headOption.exists {
      case q"$parent[$out]()" =>
        parent.syntax == superType.syntax
      case x            => false
    }


  def mkTraitF(superName: Type.Name, subTypes: Seq[Defn.Class]): Stat = {
    def decapitalize(string: String): String =
      string.head.toLower + string.tail

    val stats = subTypes.map {
      case q"..$mods class $name[..$tparams](..$fields) extends $f[$tout]()" =>
        q"def ${Term.Name(decapitalize(name.toString))}[..$tparams](..$fields): F[$tout]"
    }

    q"""
        trait ForF[F[_]] {
          ..$stats
        }
        """
  }
}

object ClassOrTraitWithOneTypeParameter {
  def unapply(any: Defn): Option[(Seq[Mod], Type.Name)] = any match {
    case t: Defn.Class if t.tparams.length == 1 => Some((t.mods, t.name))
    case t: Defn.Trait if t.tparams.length == 1 => Some((t.mods, t.name))
    case x             => None
  }
}

