package examples

import cats.data.{ Coproduct, State, StateT }
import cats.free.{ Free, Inject }
import cats.implicits._
import cats.{ Applicative, Eval, Monad }
import examples.Logging.LoggingFree
import examples.UserInteraction.UserInteractionFree

import scala.io.StdIn

@free
trait KeyValueStore[F[_]] {
  def setValue(key: String, value: String): F[Unit]

  def getValue(key: String): F[Option[String]]
}

@free
trait Logging[F[_]] {
  def debug(value: String): F[Unit]

  def info(value: String): F[Unit]
}

trait UserInteraction[F[_]] {
  def readLn(prompt: String): F[String]

  def writeLn(s: String): F[Unit]
}

object UserInteraction {
  def apply[F[_]](implicit instance: UserInteraction[F]): UserInteraction[F] = instance

  sealed abstract class UserInteractionFree[A] extends Product with Serializable

  object UserInteractionFree {

    final case class ReadLn(prompt: String) extends UserInteractionFree[String]

    final case class WriteLn(s: String) extends UserInteractionFree[Unit]

  }

  def fromFunctionK[F[_]](
    f: _root_.cats.arrow.FunctionK[UserInteractionFree, F]
  ): UserInteraction[F] = new UserInteraction[F] {
    def readLn(prompt: String): F[String] = f(UserInteractionFree.ReadLn(prompt))

    def writeLn(s: String): F[Unit] = f(UserInteractionFree.WriteLn(s))
  }

  def toFunctionK[F[_]](
    ops: UserInteraction[F]
  ): _root_.cats.arrow.FunctionK[UserInteractionFree, F] =
    new _root_.cats.arrow.FunctionK[UserInteractionFree, F] {
      def apply[A](op: UserInteractionFree[A]): F[A] = op match {
        case UserInteractionFree.ReadLn(prompt) => ops.readLn(prompt)
        case UserInteractionFree.WriteLn(s) => ops.writeLn(s)
      }
    }

  private sealed trait FreeHelper686[F[_]] {
    type Out[A] = Free[F, A]
  }

  implicit def freeInstance[F[_]](
    implicit inject: _root_.cats.free.Inject[UserInteractionFree, F]
  ): UserInteraction[FreeHelper686[F]#Out] =
    fromFunctionK(new _root_.cats.arrow.FunctionK[UserInteractionFree, FreeHelper686[F]#Out] {
      def apply[A](op: UserInteractionFree[A]): Free[F, A] = _root_.cats.free.Free.inject(op)
    })
}

object StateKeyValueStore extends KeyValueStore[State[Map[String, String], ?]] {
  override def setValue(key: String, value: String): State[Map[String, String], Unit] =
    StateT.modify[Eval, Map[String, String]](_.updated(key, value))

  override def getValue(key: String): State[Map[String, String], Option[String]] =
    StateT.inspect(_.get(key))
}

class ConsoleLogging[F[_]: Applicative] extends Logging[F] {
  override def debug(value: String): F[Unit] =
    Applicative[F].pure(print(s"DEBUG: $value\n"))

  override def info(value: String): F[Unit] =
    Applicative[F].pure(print(s"INFO: $value\n"))
}

class ConsoleUserInteraction[F[_]: Applicative] extends UserInteraction[F] {
  override def readLn(prompt: String): F[String] = Applicative[F].pure(StdIn.readLine(prompt))

  override def writeLn(s: String): F[Unit] = Applicative[F].pure(println(s))
}

object App {
  import examples.KeyValueStore._
  def main(args: Array[String]): Unit = {

    def setAndGetPreviosValue[F[_]: Monad: KeyValueStore: Logging](
      key: String,
      value: String
    ): F[Option[String]] =
      for {
        previous <- KeyValueStore[F].getValue(key)
        _ <- Logging[F].info(s"Was $key = $previous")
        _ <- Logging[F].debug(s"Setting $key to $value")
        _ <- KeyValueStore[F].setValue(key, value)
      } yield previous

    def program[F[_]: Monad: KeyValueStore: Logging: UserInteraction]: F[Unit] =
      for {
        key <- UserInteraction[F].readLn("Enter key: ")
        value <- UserInteraction[F].readLn("Enter value: ")
        previous <- setAndGetPreviosValue[F](key, value)
        _ <- UserInteraction[F].writeLn(
              previous.map(s => s"Previous value was $s").getOrElse("Previous value was not set")
            )
        exit <- UserInteraction[F].readLn("Exit? (y/n): ").map(_ == "y")
        _ <- if (exit) {
              Monad[F].pure()
            } else {
              program[F]
            }
      } yield ()

    type Algebra[A] =
      Coproduct[KeyValueStoreFree, Coproduct[LoggingFree, UserInteractionFree, ?], A]

    implicit val ev: Inject[UserInteractionFree, Algebra] = Inject.catsFreeRightInjectInstance(
      Inject.catsFreeRightInjectInstance(Inject.catsFreeReflexiveInjectInstance)
    )

    val freeProgram = program[Free[Algebra, ?]]

    val keyValueStoreFreeInterpreter = KeyValueStore.toFunctionK(StateKeyValueStore)
    val loggingFreeInterpreter =
      Logging.toFunctionK(new ConsoleLogging[State[Map[String, String], ?]])
    val userInteractionFreeInterpreter = UserInteraction.toFunctionK(
      new ConsoleUserInteraction[cats.data.State[Map[String, String], ?]]
    )

    val k =
      keyValueStoreFreeInterpreter.or(loggingFreeInterpreter.or(userInteractionFreeInterpreter))

    val out =
      freeProgram.foldMap(k)

    val result = out.run(Map.empty)

    println(result.value)

  }
}
