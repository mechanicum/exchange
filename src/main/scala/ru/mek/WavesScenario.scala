package ru.mek

object WavesScenario {
    def main(args: Array[String]): Unit = {
        val resultExchange = new Exchange(ExchangeState(IO.readClients)).processOrdersList(
            IO.readOrders,
            verbose = args.contains("verbose")
        )

        args.find(_.startsWith("out=")).map(
            (outArg: String) => IO.writeClients(resultExchange.state.clients, outArg.stripPrefix("out="))
        ).getOrElse(
            IO.writeClients(resultExchange.state.clients)
        )
    }
}
