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

    private def getHoldingOrdersByNameAndOperation(
        name: String,
        sellPapers: Boolean
    ): Map[Int, Order] = holdingOrders.filter(
        idAndOrder =>
            idAndOrder._2.sellPapers == sellPapers &&
            idAndOrder._2.name == name
    )

    private def getHoldingOrdersByPaperTypeAndOperation(
        paperType: PaperName,
        sellPapers: Boolean
    ): Map[Int, Order] = holdingOrders.filter(
        idAndOrder =>
            idAndOrder._2.paperType == paperType &&
            idAndOrder._2.sellPapers == sellPapers
    )

    private def maxHoldingOrderKey: Int = {
        val allKeys = holdingOrders.keySet

        if (allKeys.isEmpty) 0 else allKeys.max + 1
    }
}

object Exchange {
    /**
     * validates that user has enough source to process order after all his holding order would be proceeded
     *
     * @param order incoming order
     * @return option of order that is None if order is invalid
     */
    private def orderValidator(
        order: Order
    )(implicit exchange: Exchange): Option[Order] = exchange.getWallet(order.name).flatMap {
        case userState if (
            order.sellPapers &&
            userState.papers.get(order.paperType).exists(
                order.cnt + exchange.getHoldingOrdersByNameAndOperation(order.name, sellPapers = true).flatMap {
                    case (_, holdingOrder) if holdingOrder.paperType == order.paperType =>
                        Some(holdingOrder.cnt)
                    case _ => None
                }.sum <= _
            )
        ) || (
            !order.sellPapers && order.price * order.cnt + exchange.getHoldingOrdersByNameAndOperation(
                order.name, sellPapers = false
            ).flatMap {
                case (_, holdingOrder) =>
                    Some(holdingOrder.price * holdingOrder.cnt)
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
    )(implicit exchange: Exchange): Option[Order] = (
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
                        sellerState <- exchange.getWallet(seller)
                        buyerState <- exchange.getWallet(buyer)
                        sellerPaperCnt <- sellerState.papers.get(sellerOrder.paperType)
                        buyerPaperCnt <- buyerState.papers.get(sellerOrder.paperType)
                    } yield {
                        exchange.updateClient(
                            sellerOrder.name,
                            Wallet(
                                sellerState.dollars + priceDelta,
                                sellerState.papers + (
                                    sellerOrder.paperType -> (sellerPaperCnt - minCnt)
                                )
                            )
                        )

                        exchange.updateClient(
                            buyerOrder.name,
                            Wallet(
                                buyerState.dollars - priceDelta,
                                buyerState.papers + (
                                    sellerOrder.paperType -> (buyerPaperCnt + minCnt)
                                )
                            )
                        )
                        if (incomingOrder.cnt > holdingOrder.cnt) { //deal closes holding order
                            exchange.deleteOrder(holdingOrderId)

                            Some(incomingOrder.copy(cnt = incomingOrder.cnt - holdingOrder.cnt))
                        } else if (incomingOrder.cnt < holdingOrder.cnt) { //deal closes valid order
                            exchange.updateOrder(
                                holdingOrderId,
                                holdingOrder.copy(cnt = holdingOrder.cnt - incomingOrder.cnt)
                            )

                            None
                        } else { //deal closes two orders
                            exchange.deleteOrder(holdingOrderId)

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
    )(implicit exchange: Exchange): Exchange = {

        val changedIncomingOrderOpt = this.orderValidator(order).flatMap(
            validOrder =>
                (
                    Option(validOrder) /:
                    exchange.getHoldingOrdersByPaperTypeAndOperation(
                        order.paperType,
                        !order.sellPapers
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
                exchange.addOrder(orderId, changedIncomingOrder) //add incoming order if not empty
        )

        exchange
    }

    /**
     * processes list of orders changing exchange state
     *
     * @param orders list of orders to process
     * @return this Exchange
     */
    def processOrdersList(orders: Seq[Order])(implicit exchange: Exchange): Exchange = {
        val startId = exchange.maxHoldingOrderKey //for batch processing

        orders.zipWithIndex.foreach {
            case (order: Order, localId: Int) =>
                processOrder(
                    localId + startId, //TODO replace this id with something like ts + hash on input processing
                    order
                )
        }

        exchange
    }
}