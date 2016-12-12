package examples

import cats.free.Free
import cats.{Id, ~>}
import examples.FooOp.{AOp, StringOp}


@value class PurchaseOrderId
@value class UserId
@value class PurchaseOrderItem
@value class InvoiceId

@free sealed trait PurchaseOrderAggregateOp[A]

object PurchaseOrderAggregateOp {

  final case class SubmitPurchaseOrder(purchaseOrderId: PurchaseOrderId,
                                       userId: UserId,
                                       items: List[PurchaseOrderItem])
    extends PurchaseOrderAggregateOp[String Either Unit]

  final case class MarkPurchaseOrderAsInvoiced[A](purchaseOrderId: PurchaseOrderId,
                                               invoiceId: InvoiceId, l: List[A])
    extends PurchaseOrderAggregateOp[String Either A]

}


@free sealed trait FooOp[A]
object FooOp {

  final case class StringOp(string: String)
    extends FooOp[Int Either String]

  final case class AOp[A](a: A)
    extends FooOp[Either[Unit, A]]

}

@genfree trait FooOps[F[_]] {
  def stringOp(string: String): F[Int Either String]
  def aOp[A](a: A): F[Either[Unit, A]]
}


object App {
  def main(args: Array[String]): Unit = {
    @poly def fooOpToId[A](fooOp: FooOp[A]): Id[A] = fooOp match {
      case StringOp(string) => Right(string)
      case AOp(a) => Left(())
    }
    FooOps.FooOpsFree.getClass
    val free = Free.liftF[FooOp, Either[Int, String]](FooOp.StringOp("test"))
    println(free.foldMap(fooOpToId))
  }
}