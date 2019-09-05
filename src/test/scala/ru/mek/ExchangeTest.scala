package ru.mek

import org.scalatest.{Matchers, WordSpec}
import ru.mek.PaperName._

class ExchangeTest extends WordSpec with Matchers {
    "Simple validate orders" in {
        val clients = Map(
            "C1" -> Wallet(1000, Map(A -> 10, B -> 5, C -> 15, D -> 0)),
            "C2" -> Wallet(2000, Map(A -> 3, B -> 35, C -> 40, D -> 10))
        )

        val orders = Seq(
            Order("C1", sellPapers = false, A, 150, 10),
            Order("C2", sellPapers = true, B, 10, 40)
        )

        implicit val exchange: Exchange = new Exchange(clients)

        Exchange.processOrdersList(orders)

        (exchange.getClients, exchange.getHoldingOrders).shouldBe(
            (clients, Map.empty[Int, Order])
        )
    }

    "Merge orders" in {
        val clients = Map(
            "C1" -> Wallet(1000, Map(A -> 10, B -> 5, C -> 15, D -> 0)),
            "C2" -> Wallet(2000, Map(A -> 11, B -> 35, C -> 40, D -> 10))
        )

        val orders = Seq(
            Order("C1", sellPapers = false, A, 12, 10),
            Order("C2", sellPapers = true, A, 10, 8)
        )

        implicit val exchange: Exchange = new Exchange(clients)

        Exchange.processOrdersList(orders)

        (exchange.getClients, exchange.getHoldingOrders).shouldBe(
            (
                Map(
                    "C1" -> Wallet(920, Map(A -> 18, B -> 5, C -> 15, D -> 0)),
                    "C2" -> Wallet(2080, Map(A -> 3, B -> 35, C -> 40, D -> 10))
                ),
                Map(
                    0 -> Order("C1", sellPapers = false, A, 12, 2)
                )
            )
        )
    }

    "Merge multiple orders" in {
        val clients = Map(
            "C1" -> Wallet(1000, Map(A -> 10, B -> 5, C -> 15, D -> 0)),
            "C2" -> Wallet(2000, Map(A -> 11, B -> 35, C -> 40, D -> 10)),
            "C3" -> Wallet(3000, Map(A -> 20, B -> 20, C -> 42, D -> 88))
        )

        val orders = Seq(
            Order("C1", sellPapers = false, A, 10, 10),
            Order("C2", sellPapers = false, A, 10, 10),
            Order("C3", sellPapers = true, A, 10, 15)
        )

        implicit val exchange: Exchange = new Exchange(clients)

        Exchange.processOrdersList(orders)

        (exchange.getClients, exchange.getHoldingOrders).shouldBe(
            (
                Map(
                    "C1" -> Wallet(900, Map(A -> 20, B -> 5, C -> 15, D -> 0)),
                    "C2" -> Wallet(1950, Map(A -> 16, B -> 35, C -> 40, D -> 10)),
                    "C3" -> Wallet(3150, Map(A -> 5, B -> 20, C -> 42, D -> 88))
                ),
                Map(
                    1 -> Order("C2", sellPapers = false, A, 10, 5)
                )
            )
        )
    }

    "Cannot buy for price lower then sells" in {
        val clients = Map(
            "C1" -> Wallet(1000, Map(A -> 10, B -> 5, C -> 15, D -> 0)),
            "C2" -> Wallet(2000, Map(A -> 11, B -> 35, C -> 40, D -> 10))
        )

        val orders = Seq(
            Order("C1", sellPapers = false, A, 10, 10),
            Order("C2", sellPapers = true, A, 12, 8)
        )

        implicit val exchange: Exchange = new Exchange(clients)

        Exchange.processOrdersList(orders)

        (exchange.getClients, exchange.getHoldingOrders).shouldBe(
            (
                clients,
                Map(
                    0 -> Order("C1", sellPapers = false, A, 10, 10),
                    1 -> Order("C2", sellPapers = true, A, 12, 8)
                )
            )
        )
    }

    "Validate orders with mind of all users holding orders by dollars" in {
        val clients = Map(
            "C1" -> Wallet(1000, Map(A -> 10, B -> 5, C -> 15, D -> 0)),
            "C2" -> Wallet(2000, Map(A -> 16, B -> 35, C -> 40, D -> 10))
        )

        val orders = Seq(
            Order("C1", sellPapers = false, A, 90, 10),
            Order("C1", sellPapers = false, A, 90, 10),
            Order("C2", sellPapers = true, A, 90, 15)
        )

        implicit val exchange: Exchange = new Exchange(clients)

        Exchange.processOrdersList(orders)

        (exchange.getClients, exchange.getHoldingOrders).shouldBe(
            (
                Map(
                    "C1" -> Wallet(100, Map(A -> 20, B -> 5, C -> 15, D -> 0)),
                    "C2" -> Wallet(2900, Map(A -> 6, B -> 35, C -> 40, D -> 10))
                ),
                Map(
                    2 -> Order("C2", sellPapers = true, A, 90, 5)
                )
            )
        )
    }

    "Validate orders with mind of all users holding orders by paper count" in {
        val clients = Map(
            "C1" -> Wallet(1000, Map(A -> 30, B -> 5, C -> 15, D -> 0)),
            "C2" -> Wallet(2000, Map(A -> 16, B -> 35, C -> 40, D -> 10))
        )

        val orders = Seq(
            Order("C1", sellPapers = true, A, 90, 20),
            Order("C1", sellPapers = true, A, 90, 20),
            Order("C2", sellPapers = false, A, 90, 10)
        )

        implicit val exchange: Exchange = new Exchange(clients)

        Exchange.processOrdersList(orders)

        (exchange.getClients, exchange.getHoldingOrders).shouldBe(
            (
                Map(
                    "C1" -> Wallet(1900, Map(A -> 20, B -> 5, C -> 15, D -> 0)),
                    "C2" -> Wallet(1100, Map(A -> 26, B -> 35, C -> 40, D -> 10))
                ),
                Map(
                    0 -> Order("C1", sellPapers = true, A, 90, 10)
                )
            )
        )
    }
    
    "Update does not break anything" in {
        val clients = IO.readClients
        
        val orders = IO.readOrders

        implicit val exchange: Exchange = new Exchange(clients)

        Exchange.processOrdersList(orders)

        exchange.getClients.shouldBe(
            Map(
                "C1" -> Wallet(3582, Map(A -> 158, B -> 106, C -> 658, D -> 166)),
                "C2" -> Wallet(2251, Map(A -> 453, B -> 328, C -> 926, D -> 619)),
                "C3" -> Wallet(17, Map(A -> 92, B -> 126, C -> 50, D -> 97)),
                "C4" -> Wallet(10094, Map(A -> 255, B -> 52, C -> 407, D -> 125)),
                "C5" -> Wallet(519, Map(A -> 74, B -> 96, C -> 355, D -> 159)),
                "C6" -> Wallet(3137, Map(A -> 621, B -> 127, C -> 184, D -> 187)),
                "C7" -> Wallet(849, Map(A -> 76, B -> 60, C -> 685, D -> 69)),
                "C8" -> Wallet(389, Map(A -> 188, B -> 413, C -> 172, D -> 451)),
                "C9" -> Wallet(772, Map(A -> 363, B -> 473, C -> 130, D -> 592))
            )
        )
    }
}
