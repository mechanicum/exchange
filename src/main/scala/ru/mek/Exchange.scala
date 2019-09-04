package ru.mek

case class Wallet(dollars: Int, papers: Map[PaperName, Int])

case class Order(name: String, sellPapers: Boolean, paperType: PaperName, price: Int, cnt: Int)

class Exchange(clientsInit: Map[String, Wallet], holdingOrdersInit: Map[Int, Order] = Map.empty) {
    //TODO replace vars with work with storage
    private var clients: Map[String, Wallet] = clientsInit

    private var holdingOrders: Map[Int, Order] = holdingOrdersInit

    def getClients: Map[String, Wallet] = clients

    def getHoldingOrders: Map[Int, Order] = holdingOrders

    private def getWallet(name: String): Option[Wallet] = clients.get(name)

    private def updateClient(name: String, wallet: Wallet): Unit =
        clients = clients + (name -> wallet)

    private def deleteOrder(id: Int): Unit =
        holdingOrders = holdingOrders - id

    private def updateOrder(id: Int, order: Order): Unit =
        holdingOrders = holdingOrders + (id -> order)

    private def addOrder(id: Int, order: Order): Unit =
        updateOrder(id, order)

    /**
     * validates that user has enough source to process order after all his holding order would be proceeded
     *
     * @param order incoming order
     * @return option of order that is None if order is invalid
     */
    private def orderValidator(
        order: Order
    ): Option[Order] = this.getWallet(order.name).flatMap {
        case userState if (
            order.sellPapers &&
            userState.papers.get(order.paperType).exists(
                order.cnt + this.holdingOrders.flatMap {
                    case (_, holdingOrder) if holdingOrder.sellPapers &&
                                              holdingOrder.name == order.name &&
                                              holdingOrder.paperType == order.paperType
                    => Some(holdingOrder.cnt)
                    case _ => None
                }.sum <= _
            )
        ) || (
            !order.sellPapers && order.price * order.cnt + this.holdingOrders.flatMap {
                case (_, holdingOrder) if !holdingOrder.sellPapers &&
                                          holdingOrder.name == order.name
                => Some(holdingOrder.price * holdingOrder.cnt)
                case _ => None
            }.sum <= userState.dollars
        ) => Some(order)
        case _ => None //TODO send to client message
    }

    /**
     * merges two orders and applies result to exchange state
     *
     * @param incomingOrder  incoming order
     * @param holdingOrderId id of order from list of holding orders
     * @param holdingOrder   order from list of holding orders
     * @return changed incoming order
     */
    private def orderMerge(
        incomingOrder: Order,
        holdingOrderId: Int,
        holdingOrder: Order
    ): Option[Order] = (
        if (incomingOrder.paperType == holdingOrder.paperType && incomingOrder.sellPapers != holdingOrder.sellPapers)
            (
                if (incomingOrder.sellPapers && incomingOrder.price <= holdingOrder.price) //seller in incomingOrder
                    Some((incomingOrder, holdingOrder))
                else if (holdingOrder.sellPapers && incomingOrder.price >= holdingOrder.price) //seller in holdingOrder
                    Some((holdingOrder, incomingOrder))
                else //no deal
                    None
            ).flatMap {
                case (sellerOrder, buyerOrder) =>
                    val seller = sellerOrder.name
                    val buyer = buyerOrder.name
                    val minCnt = Seq(sellerOrder.cnt, buyerOrder.cnt).min
                    val priceDelta = sellerOrder.price * minCnt

                    for {
                        sellerState <- this.getWallet(seller)
                        buyerState <- this.getWallet(buyer)
                        sellerPaperCnt <- sellerState.papers.get(sellerOrder.paperType)
                        buyerPaperCnt <- buyerState.papers.get(sellerOrder.paperType)
                    } yield {
                        this.updateClient(
                            sellerOrder.name,
                            Wallet(
                                sellerState.dollars + priceDelta,
                                sellerState.papers + (
                                    sellerOrder.paperType -> (sellerPaperCnt - minCnt)
                                )
                            )
                        )

                        this.updateClient(
                            buyerOrder.name,
                            Wallet(
                                buyerState.dollars - priceDelta,
                                buyerState.papers + (
                                    sellerOrder.paperType -> (buyerPaperCnt + minCnt)
                                )
                            )
                        )
                        if (incomingOrder.cnt > holdingOrder.cnt) { //deal closes holding order
                            this.deleteOrder(holdingOrderId)

                            Some(incomingOrder.copy(cnt = incomingOrder.cnt - holdingOrder.cnt))
                        } else if (incomingOrder.cnt < holdingOrder.cnt) { //deal closes valid order
                            this.updateOrder(
                                holdingOrderId,
                                holdingOrder.copy(cnt = holdingOrder.cnt - incomingOrder.cnt)
                            )

                            None
                        } else { //deal closes two orders
                            this.deleteOrder(holdingOrderId)

                            None
                        }
                    }
            }
        else
            None
    ).getOrElse(Some(incomingOrder)) //if no deal

    /**
     * processes one order changing exchange state
     *
     * @param orderId id of order in this batch
     * @param order   order to process
     * @return this Exchange
     */
    def processOrder(
        orderId: Int,
        order: Order
    ): Exchange = {

        val changedIncomingOrderOpt = this.orderValidator(order).flatMap(
            validOrder =>
                (
                    Option(validOrder) /:
                    this.holdingOrders.filter(
                        idAndOrder =>
                            idAndOrder._2.sellPapers != order.sellPapers &&
                            idAndOrder._2.paperType == order.paperType
                    ).toSeq.sortBy(_._1)
                ) {
                    case (
                        incomingOrderOpt: Option[Order],
                        (holdingOrderId: Int, holdingOrder: Order)
                    ) =>
                        incomingOrderOpt.flatMap {
                            incomingOrder =>
                                this.orderMerge(incomingOrder, holdingOrderId, holdingOrder)
                        }
                }
        )

        changedIncomingOrderOpt.foreach(
            changedIncomingOrder =>
                this.addOrder(orderId, changedIncomingOrder) //add incoming order if not empty
        )

        this
    }

    /**
     * processes list of orders changing exchange state
     *
     * @param orders list of orders to process
     * @return this Exchange
     */
    def processOrdersList(orders: Seq[Order]): Exchange = {
        val oldKeys = holdingOrders.keySet //for batch processing

        val startId = if (oldKeys.isEmpty) 0 else oldKeys.max + 1

        orders.zipWithIndex.foreach {
            case (order: Order, localId: Int) =>
                this.processOrder(
                    localId + startId, //TODO replace this id with something like ts + hash on input processing
                    order
                )
        }

        this
    }
}