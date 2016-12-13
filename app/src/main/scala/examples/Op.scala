package examples

import cats.Eval
import cats.data.{ State, StateT }
import cats.free.Free

@value class PurchaseOrderId
@value class UserId
@value class PurchaseOrderItem
@value class InvoiceId

@free
trait KeyValueStore[F[_]] {
  def setValue(key: String, value: String): F[Unit]
  def getValue(key: String): F[Option[String]]
}

object App {
  import examples.KeyValueStore._
  def main(args: Array[String]): Unit = {

    def kvState[A](fooOp: KeyValueStore.KeyValueStoreFree[A]): State[Map[String, String], A] =
      fooOp match {
        case KeyValueStoreFree.SetValue(key, value) =>
          StateT.modify[Eval, Map[String, String]](_.updated(key, value))
        case KeyValueStoreFree.GetValue(key) => StateT.inspect(_.get(key))
      }

    val freeProgram = Free.liftF(KeyValueStoreFree.SetValue("k", "v"))
//    println(freeProgram.foldMap(kvState))
  }
}
