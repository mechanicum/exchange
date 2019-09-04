package ru.mek

case class Wallet(dollars: Int, papers: Map[PaperName, Int])

case class Order(name: String, sellPapers: Boolean, paperType: PaperName, price: Int, cnt: Int)

class Exchange(val clients: Map[String, Wallet], val holdingOrders: Map[Int, Order] = Map.empty) {
    //TODO replace with work with storage
    def getWallet(name: String): Option[Wallet] = clients.get(name)

    def updateClient(name: String, wallet: Wallet): Exchange =
        new Exchange(clients + (name -> wallet), holdingOrders)

    def deleteOrder(id: Int): Exchange =
        new Exchange(clients, holdingOrders - id)

    def updateOrder(id: Int, order: Order): Exchange =
        new Exchange(clients, holdingOrders + (id -> order))

    def addOrder(id: Int, order: Order): Exchange =
        updateOrder(id, order)

    /**
     * validates that user has enough source to process order after all his holding order would be proceeded
     *
     * @param order    incoming order
     * @return option of order that is None if order is invalid
     */
    def orderValidator(
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
     * @return new exchange state and changed incoming order
     */
    def orderMerge(
        incomingOrder: Order,
        holdingOrderId: Int,
        holdingOrder: Order
    ): (Exchange, Option[Order]) = (
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
                        newState = this.updateClient(
                            sellerOrder.name,
                            Wallet(
                                sellerState.dollars + priceDelta,
                                sellerState.papers + (
                                    sellerOrder.paperType -> (sellerPaperCnt - minCnt)
                                )
                            )
                        ).updateClient(
                            buyerOrder.name,
                            Wallet(
                                buyerState.dollars - priceDelta,
                                buyerState.papers + (
                                    sellerOrder.paperType -> (buyerPaperCnt + minCnt)
                                )
                            )
                        )
                    } yield
                        if (incomingOrder.cnt > holdingOrder.cnt) //deal closes holding order
                            (
                                newState.deleteOrder(holdingOrderId),
                                Some(incomingOrder.copy(cnt = incomingOrder.cnt - holdingOrder.cnt))
                            )
                        else if (incomingOrder.cnt < holdingOrder.cnt) //deal closes valid order
                            (
                                newState.updateOrder(
                                    holdingOrderId,
                                    holdingOrder.copy(cnt = holdingOrder.cnt - incomingOrder.cnt)
                                ),
                                None
                            )
                        else //deal closes two orders
                            (newState.deleteOrder(holdingOrderId), None)
            }
        else
            None
    ).getOrElse((this, Some(incomingOrder))) //if no deal

    /**
     * processes one order changing exchange state
     *
     * @param localId id of order in this batch
     * @param order order to process
     * @param verbose optional, default false, if true prints to stdout how much orders from batch is done
     * @param startId optional, default 0, id to merge with indices from previous batch
     * @param ordersCount optional, default 0, for verbose count of all orders
     * @return new state after processing this order
     */
    def processOrder(
        localId: Int,
        order: Order
    )(verbose: Boolean = false, startId: Int = 0, ordersCount: Int = 0): Exchange = {
        if (verbose) print(s"\r${localId + 1} of $ordersCount")

        val orderId = localId + startId //TODO replace this id with something like ts + hash on input processing

        val (changedExchange, changedIncomingOrderOpt) = this.orderValidator(order).map(
            validOrder =>
                (
                    (this, Option(validOrder)) /:
                    this.holdingOrders.filter(
                        idAndOrder =>
                            idAndOrder._2.sellPapers != order.sellPapers &&
                            idAndOrder._2.paperType == order.paperType
                    ).toSeq.sortBy(_._1)
                ) {
                    case (
                        (exchange: Exchange, incomingOrderOpt: Option[Order]),
                        (holdingOrderId: Int, holdingOrder: Order)
                    ) =>
                        incomingOrderOpt.map {
                            incomingOrder =>
                                exchange.orderMerge(incomingOrder, holdingOrderId, holdingOrder)
                        }.getOrElse((exchange, incomingOrderOpt))
                }
        ).getOrElse((this, None))

        changedIncomingOrderOpt.map(
            changedIncomingOrder =>
                changedExchange.addOrder(orderId, changedIncomingOrder) //add incoming order if not empty
        ).getOrElse(changedExchange)
    }

    /**
     * processes list of orders changing exchange state
     *
     * @param orders list of orders to process
     * @param verbose optional, default false, if true prints to stdout how much is done
     * @return new state after processing all orders
     */
    def processOrdersList(orders: Seq[Order], verbose: Boolean = false): Exchange = {
        val oldKeys = holdingOrders.keySet //for batch processing

        val startId = if (oldKeys.isEmpty) 0 else oldKeys.max + 1

        val ordersCount = if (verbose) orders.size else 0

        val result = (this /: orders.zipWithIndex) {
            case (exchange: Exchange, (order: Order, localId: Int)) =>
                exchange.processOrder(localId, order)(verbose, startId, ordersCount)
        }

        if (verbose) println()

        result
    }
}