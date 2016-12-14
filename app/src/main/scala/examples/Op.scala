package examples

import cats.data.{ State, StateT }
import cats.free.Free
import cats.implicits._
import cats.{ Eval, Monad }

@value class PurchaseOrderId
@value class UserId
@value class PurchaseOrderItem
@value class InvoiceId

@free
trait KeyValueStore[F[_]] {
  def setValue(key: String, value: String): F[Unit]

  def getValue(key: String): F[Option[String]]
}

object StateKeyValueStore extends KeyValueStore[State[Map[String, String], ?]] {
  override def setValue(key: String, value: String): State[Map[String, String], Unit] =
    StateT.modify[Eval, Map[String, String]](_.updated(key, value))

  override def getValue(key: String): State[Map[String, String], Option[String]] =
    StateT.inspect(_.get(key))
}

object App {
  import examples.KeyValueStore._
  def main(args: Array[String]): Unit = {

    def program[F[_]: Monad: KeyValueStore]: F[Option[String]] =
      for {
        _ <- KeyValueStore[F].setValue("key", "test")
        value <- KeyValueStore[F].getValue("key")
      } yield value

    val k =
      KeyValueStore.toFunctionK(StateKeyValueStore)

    val freeProgram = program[Free[KeyValueStoreFree, ?]]

    val out =
      freeProgram.foldMap(k)
  }
}
