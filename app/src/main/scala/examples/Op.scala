package examples


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

@main object App {
  new PurchaseOrderAggregateOp.ForF[List] {
    def submitPurchaseOrder(purchaseOrderId: PurchaseOrderId, userId: UserId, items: List[PurchaseOrderItem]): List[Either[String, Unit]] = ???
    def markPurchaseOrderAsInvoiced[A](purchaseOrderId: PurchaseOrderId, invoiceId: InvoiceId, l: List[A]): List[Either[String, A]] = ???
  }
}