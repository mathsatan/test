package com.kruchkov.numbers


import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success, Try}

object Diophantine {
  type FactorTuple = (List[Int], List[Int])

  def gen(list: List[FactorTuple]): Try[List[(Int, Int)]] = {
    if (list == null || list.isEmpty) return Failure(new IllegalArgumentException("Empty list"))
    val zipped = zip(list)
    val result: ListBuffer[(Int, Int)] = ListBuffer[(Int, Int)]()
    zipped.foreach(tuple => {
      val a: Int = tuple._1
      val b: Int = tuple._2
      if ((b - a) % 4 == 0) {
        val x: Int = a + (b - a)/2
        val y: Int = (b - a)/4
        result += Tuple2[Int, Int](x, y)
      }
    })
    Success(result.toList)
  }

  def twoBoxCombs(list: List[Int]): Try[List[FactorTuple]] = {
    if (list == null || list.isEmpty) return Failure(new IllegalArgumentException("Empty list"))
    Success(comb(list.sortWith(_ < _), (Nil, Nil)))
  }

  private def comb(list: List[Int], tuple: FactorTuple): List[FactorTuple] = list match {
    case h :: t => {
      val start = list.indexOf(h)
      val end = list.lastIndexOf(h)
      val power = end - start + 1
      if (power < 2) {
        comb(t, (h :: tuple._1, tuple._2)) ::: comb(t, (tuple._1, h :: tuple._2))
      } else {
        val l = List.fill(power)(h)
        val result: ListBuffer[FactorTuple] = ListBuffer[FactorTuple]()
        val tail = t.slice(end, t.size)
        (0 to l.size).foreach((i) => {
          result ++= comb(tail, (l.slice(0, i) ::: tuple._1, tuple._2 ::: l.slice(i, l.size)))
        })
        result.toList
      }
    }
    case _ => List[FactorTuple](tuple)
  }

  def zip(list: List[FactorTuple]): List[(Int, Int)] = list.map(x => (x._1.product, x._2.product))

  def eval(n: Int): List[(Int, Int)] = {
    val solutions = for {
      factList  <- Primes.factorize(n)
      lst       <- twoBoxCombs(factList)
      result    <- gen(lst)
    } yield result

    solutions match {
      case Success(list) => list
      case Failure(e) => {
        println(e.getMessage)
        Nil
      }
    }
  }

    /*type FactorList = (List[PrimeFactor], List[PrimeFactor])
    def oldComb(list: List[PrimeFactor], a: FactorList): List[FactorList] = list match {
      case h :: t => if (h.power < 2) {
        comb(t, (h :: a._1, a._2)) ::: comb(t, (a._1, h :: a._2))
      } else {
        val l = List.fill(h.power)(PrimeFactor(h.n, 1))
        //g(l, a)

        val result: ListBuffer[FactorList] = ListBuffer[FactorList]()
        (0 to l.size).foreach((i) => {
          result ++= comb(t, (l.slice(0, i) ::: a._1, a._2 ::: l.slice(i, l.size)))
        })
        result.toList
      }
      case _ => List[FactorList](a)
    }*/
}
