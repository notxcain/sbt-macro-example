package examples

import cats.data.{ Coproduct, State, StateT }
import cats.free.Free
import cats.implicits._
import cats.{ Applicative, Eval, Monad }
import examples.Logging.LoggingFree

@value class PurchaseOrderId
@value class UserId
@value class PurchaseOrderItem
@value class InvoiceId

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

object App {
  import examples.KeyValueStore._
  def main(args: Array[String]): Unit = {

    def program[F[_]: Monad: KeyValueStore: Logging]: F[Option[String]] =
      for {
        _ <- KeyValueStore[F].setValue("env", "test")
        _ <- Logging[F].debug("I set env = test")
        value <- KeyValueStore[F].getValue("env")
        _ <- Logging[F].info(s"I fetched $value")
      } yield value

    val freeProgram = program[Free[Coproduct[KeyValueStoreFree, LoggingFree, ?], ?]]

    val k =
      KeyValueStore
        .toFunctionK(StateKeyValueStore)
        .or(Logging.toFunctionK(new ConsoleLogging[State[Map[String, String], ?]]))

    val out =
      freeProgram.foldMap(k)

    val result = out.run(Map.empty)

    println(result.value)

  }
}
