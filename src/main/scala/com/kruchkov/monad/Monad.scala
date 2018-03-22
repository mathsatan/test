package com.kruchkov.monad

import scala.util.{Failure, Success, Try}

object Monad {
  def f1() = {
    for {
      n <- List(1, 2)
      ch <- List('a', 'b')
    } yield (n, ch)
  }

  def f2() = {
    (1 to 2).flatMap(
      x => ('a' to 'b').map(y => (x, y))
    )
  }

  def g1() = {
    for {
      h <- getHost()
      conn <- connect(h)
    } yield conn
  }

  def g2() = {
    getHost().flatMap(
      hostName => connect(hostName).map(y => y)
    )
  }

  def getHost(): Try[String] =
    Failure(new Exception("getting host fail"))//Success[String]("host_name")
  def connect(h: String): Try[String] =
    if ("host_name" == h)
      Success[String]("Connected!")
    else
      Failure(new Exception("connection fail"))
}
