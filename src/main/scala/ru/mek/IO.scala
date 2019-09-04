package ru.mek

import java.io.{File, PrintWriter}

import ru.mek.PaperName._

import scala.io.Source.fromInputStream
import scala.util.Try

object IO {
    def readClients: Map[String, Wallet] = (
        for {
            line <- fromInputStream(getClass.getResourceAsStream("/clients.txt")).getLines
            strData = line.split("\t")
            name <- strData.lift(0)
            dollars <- strData.lift(1).flatMap(str => Try(str.toInt).toOption)
            paper0 <- strData.lift(2).flatMap(str => Try(str.toInt).toOption)
            paper1 <- strData.lift(3).flatMap(str => Try(str.toInt).toOption)
            paper2 <- strData.lift(4).flatMap(str => Try(str.toInt).toOption)
            paper3 <- strData.lift(5).flatMap(str => Try(str.toInt).toOption)
        } yield {
            (
                name,
                Wallet(dollars, Map(A -> paper0, B -> paper1, C -> paper2, D -> paper3))
            )
        }
    ).toMap

    def readOrders: Seq[Order] = (
        for {
            line <- fromInputStream(getClass.getResourceAsStream("/orders.txt")).getLines
            strData = line.split("\t")
            name <- strData.lift(0)
            operation <- strData.lift(1).flatMap {
                case "s" => Some(true)
                case "b" => Some(false)
                case _ => None
            }
            paperType <- strData.lift(2).flatMap {
                case "A" => Some(A)
                case "B" => Some(B)
                case "C" => Some(C)
                case "D" => Some(D)
                case _ => None
            }
            price <- strData.lift(3).flatMap(str => Try(str.toInt).toOption)
            cnt <- strData.lift(4).flatMap(str => Try(str.toInt).toOption)
        } yield {
            Order(name, operation, paperType, price, cnt)
        }
    ).toSeq

    def writeClients(clients: Map[String, Wallet], fPath: String = "./target/result.txt"): Unit = {
        val pw = new PrintWriter(new File(fPath))
        pw.write(
            clients.toSeq.sortBy(_._1).map {
                case (name, wallet) =>
                    Seq(
                        name,
                        wallet.dollars,
                        wallet.papers.getOrElse(A, 0),
                        wallet.papers.getOrElse(B, 0),
                        wallet.papers.getOrElse(C, 0),
                        wallet.papers.getOrElse(D, 0)
                    ).mkString("\t")
            }.mkString("\n")
        )
        pw.close()
    }
}
