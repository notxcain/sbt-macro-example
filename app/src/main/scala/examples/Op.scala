package examples

import examples.FooOp.ForF


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
    extends FooOp[String]

  final case class AOp[A](a: A)
    extends FooOp[A]

}

@main object App {
  val f = new ForF[List] {
    def stringOp(string: String): List[String] = Nil
    def aOp[A](a: A): List[A] = Nil
  }
}