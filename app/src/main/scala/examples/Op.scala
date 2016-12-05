package examples

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
    extends FooOp[Unit Either A]

}

@main object App {
  @poly def fooOpToId[A](fooOp: FooOp[A]): Id[A] = fooOp match {
    case StringOp(string) => Right(string)
    case AOp(a) => Left(())
  }
}