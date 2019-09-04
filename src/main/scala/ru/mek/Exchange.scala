package ru.mek

case class Wallet(dollars: Int, papers: Map[PaperName, Int])
case class Order(name: String, sellPapers: Boolean, paperType: PaperName, price: Int, cnt: Int)
case class ExchangeState(clients: Map[String, Wallet], holdingOrders: Map[Int, Order] = Map.empty) //TODO replace Map in clients with connector that have method get(name: String): Option[Wallet] and T + (String -> Wallet) => T

class Exchange(val state: ExchangeState) {

    /***
     * validates that user has enough source to process order after all his holding order would be proceeded
     * @param order incoming order
     * @param exchange state of exchange before processing this orders
     * @return option of order that is None if order is invalid
     */
    def orderValidator(
        order: Order,
        exchange: ExchangeState
    ): Option[Order] = exchange.clients.get(order.name).flatMap {
        case userState if (
                              order.sellPapers &&
                              userState.papers.get(order.paperType).exists(
                                  order.cnt + exchange.holdingOrders.flatMap {
                                      case (_, holdingOrder) if holdingOrder.sellPapers &&
                                                                 holdingOrder.name == order.name &&
                                                                 holdingOrder.paperType == order.paperType
                                      => Some(holdingOrder.cnt)
                                      case _ => None
                                  }.sum <= _
                              )
                          ) || (
                              !order.sellPapers && order.price * order.cnt + exchange.holdingOrders.flatMap {
                                  case (_, holdingOrder) if !holdingOrder.sellPapers &&
                                                            holdingOrder.name == order.name
                                  => Some(holdingOrder.price * holdingOrder.cnt)
                                  case _ => None
                              }.sum <= userState.dollars
                          )
               => Some(order)
        case _ => None //TODO send to client message
    }

    /***
     * processes two orders and applies result to clients
     * @param incomingOrder incoming order
     * @param holdingOrder order from list of holding orders
     * @param clients state of all clients on exchange before processing this orders
     * @return changed orders and new clients state
     */
    def orderMerge(
        incomingOrder: Order,
        holdingOrder : Order,
        clients      : Map[String, Wallet]
    ): (Option[Order], Option[Order], Map[String, Wallet]) = (
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
                        sellerState <- clients.get(seller)
                        buyerState <- clients.get(buyer)
                        sellerPaperCnt <- sellerState.papers.get(sellerOrder.paperType)
                        buyerPaperCnt <- buyerState.papers.get(sellerOrder.paperType)
                        newClients = clients + (
                            sellerOrder.name -> Wallet(
                                sellerState.dollars + priceDelta,
                                sellerState.papers + (
                                    sellerOrder.paperType -> (sellerPaperCnt - minCnt)
                                )
                            )
                        ) + (
                            buyerOrder.name -> Wallet(
                                buyerState.dollars - priceDelta,
                                buyerState.papers + (
                                    sellerOrder.paperType -> (buyerPaperCnt + minCnt)
                                )
                            )
                        )
                    } yield
                        if (incomingOrder.cnt > holdingOrder.cnt) //deal closes holding order
                            (
                                Some(
                                    incomingOrder.copy(
                                        cnt = incomingOrder.cnt - holdingOrder.cnt
                                    )
                                ),
                                None,
                                newClients
                            )
                        else if (incomingOrder.cnt < holdingOrder.cnt) //deal closes valid order
                            (
                                None,
                                Some(
                                    holdingOrder.copy(
                                        cnt = holdingOrder.cnt - incomingOrder.cnt
                                    )
                                ),
                                newClients
                            )
                        else //deal closes two orders
                            (None, None, newClients)

                }
            else
                None
            ).getOrElse((Some(incomingOrder), Some(holdingOrder), clients)) //if no deal

    /***
     * processes list of orders changing exchange state
     * @param orders list of orders to process
     * @return new Exchange
     */
    def processOrdersList(orders: Seq[Order], verbose: Boolean = false): Exchange = {
        val ordersCount = if (verbose) orders.size else 0

        val oldKeys = state.holdingOrders.keySet

        val startId = if (oldKeys.isEmpty)
            0
        else
            oldKeys.max + 1

        val ordersWithIndices = orders.zipWithIndex.map {
            case (order: Order, id: Int) =>
                (id + startId, order) //TODO replace this id with something like ts + hash on input processing
        }

        val result = new Exchange(
            (state /: ordersWithIndices.zipWithIndex) {
                case (initialExchangeState: ExchangeState, ((orderId: Int, order: Order), localId: Int)) => //input - order and what is holding
                    if (verbose) print(s"\r${localId + 1} of $ordersCount")

                    val (
                        changedIncomingOrder,
                        newClients,
                        changedHoldingOrdersCollection
                    ) = orderValidator(order, initialExchangeState).map(
                        validOrder =>
                            (
                                (Option(validOrder), initialExchangeState.clients, Map.empty[Int, Option[Order]]) /:
                                initialExchangeState.holdingOrders.filter(
                                    idAndOrder =>
                                        idAndOrder._2.sellPapers != order.sellPapers &&
                                        idAndOrder._2.paperType == order.paperType
                                ).toSeq.sortBy(_._1) //leave only interesting
                            ) { //input - changed order after merges and holding that worked up
                                case (
                                    (
                                        incomingOrderOpt: Option[Order],
                                        clientState: Map[String, Wallet],
                                        changedHoldingOrders: Map[Int, Option[Order]]
                                    ),
                                    holdingOrder: (Int, Order)
                                ) =>
                                    incomingOrderOpt.map {
                                        incomingOrder =>

                                            val (newIncomingOrder, newHoldingOrder, newClients) = orderMerge(
                                                incomingOrder,
                                                holdingOrder._2,
                                                clientState
                                            )

                                            (
                                                newIncomingOrder,
                                                newClients,
                                                if (!newHoldingOrder.contains(holdingOrder._2))
                                                    changedHoldingOrders + (holdingOrder._1 -> newHoldingOrder)
                                                else
                                                    changedHoldingOrders
                                            )
                                    }.getOrElse((incomingOrderOpt, clientState, changedHoldingOrders))
                            }
                    ).getOrElse((None, initialExchangeState.clients, Map.empty[Int, Option[Order]]))

                    ExchangeState(
                        newClients,
                        changedIncomingOrder.map(
                            cio => initialExchangeState.holdingOrders + (orderId -> cio) //add incoming order if not empty
                        ).getOrElse(initialExchangeState.holdingOrders) -- //update by id holdingOrders
                        changedHoldingOrdersCollection.filter(_._2.isEmpty).keySet ++
                        changedHoldingOrdersCollection.flatMap {
                            case (key: Int, valueOpt: Option[Order]) =>
                                valueOpt.map((key, _))
                        }
                    )
            }
        )

        if (verbose) println()

        result
    }
}