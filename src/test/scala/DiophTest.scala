import com.kruchkov.numbers.{Diophantine, PrimeFactor}
import org.scalatest.FlatSpec

import scala.collection.mutable.ListBuffer
import scala.util.{Failure, Success}

class DiophTest extends FlatSpec {
  def primePower(list: List[Int], a: Int): Int = list.foldLeft(0)((acc, item) => if (item == a) acc + 1 else acc)

  def toPrimeFactList(list: List[Int]): List[PrimeFactor] = {
    val result: ListBuffer[PrimeFactor] = ListBuffer[PrimeFactor]()
    list.distinct.foreach(x => {
      result += PrimeFactor(x, primePower(list, x))
    })
    result.toList
  }

  "A combination generator" should "return Nil" in {
    Diophantine.twoBoxCombs(null) match {
      case Success(_) => fail("null argument should return Failure")
      case Failure(e) => println(e.getMessage)
    }
    Diophantine.twoBoxCombs(List()) match {
      case Success(_) => fail("Empty list argument should return Failure")
      case Failure(e) => println(e.getMessage)
    }
  }

  def combTest(list: List[Int]): Unit = {
    val number = list.product
    println(s"Number for combination: $number")
    val combs = Diophantine.twoBoxCombs(list)
    val unique: ListBuffer[String] = ListBuffer[String]()
    combs match {
      case Failure(e) => fail("Generation fails with exception: " + e.getMessage)
      case Success(lst) => {
        lst.foreach(i => {
          val p1: Int = if (i._1.isEmpty) 1 else i._1.product
          val p2: Int = if (i._2.isEmpty) 1 else i._2.product
          val m = p1 * p2
          assert(number == m, "Generated product doesn\'t match original number")
          val s = s"(${i._1.sortWith(_ < _).mkString("*")},${i._2.sortWith(_ < _).mkString("*")})"
          if (unique.contains(s)) {
            fail("Combination duplicate")
          } else {
            unique += s
          }
          println(s)
        })
        val pfList = toPrimeFactList(list)
        println(s"Combs for $number")
        val count: Int = pfList.foldLeft(1)(
          (acc, item) => {
            if (item.power > 1) acc * (item.power + 1) else acc * 2
          })
        println(s"Count should be: $count")
        assert(lst.lengthCompare(count) == 0)
      }
    }
  }

  "A combination generator" should "return all unique" in {
    combTest(List(5, 4, 5, 5))
    combTest(List(2))
    combTest(List(6, 2, 4, 2))
  }

  "Full generations" should "return result" in {
    /*Diophantine.eval(64).foreach(i =>
      println(s"(${i._1}, ${i._2})")
    )*/
  }

  "Test eval()" should "return Nil" in {
    assert(Diophantine.eval(0) == Nil)
    assert(Diophantine.eval(-1) == Nil)
    assert(Diophantine.eval(1) == Nil)
    Diophantine.eval(2).foreach(i =>
      println(s"(${i._1}, ${i._2})")
    )
  }
}
