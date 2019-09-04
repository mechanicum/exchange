package ru.mek

object WavesScenario {
    def main(args: Array[String]): Unit = {
        val resultExchange = new Exchange(IO.readClients).processOrdersList(IO.readOrders)

        args.find(_.startsWith("out=")).map(
            (outArg: String) => IO.writeClients(resultExchange.getClients, outArg.stripPrefix("out="))
        ).getOrElse(
            IO.writeClients(resultExchange.getClients)
        )
    }
}
