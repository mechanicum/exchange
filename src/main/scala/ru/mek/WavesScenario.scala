package ru.mek

object WavesScenario {
    def main(args: Array[String]): Unit = {
        implicit val exchange: Exchange = new Exchange(IO.readClients)

        Exchange.processOrdersList(IO.readOrders)

        args.find(_.startsWith("out=")).map(
            (outArg: String) => IO.writeClients(exchange.getClients, outArg.stripPrefix("out="))
        ).getOrElse(
            IO.writeClients(exchange.getClients)
        )
    }
}
