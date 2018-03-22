import com.kruchkov.numbers.{Diophantine, PrimeFactor}
import org.scalatest.FlatSpec

import scala.collection.mutable.ListBuffer

class DiophTest extends FlatSpec {
  def primePower(list: List[Int], a: Int): Int = list.foldLeft(0)((acc, item) => if (item == a) acc + 1 else acc)

  def toPrimeFactList(list: List[Int]): List[PrimeFactor] = {
    val result: ListBuffer[PrimeFactor] = ListBuffer[PrimeFactor]()
    list.distinct.foreach(x => {
      result += PrimeFactor(x, primePower(list, x))
    })
    result.toList
  }

  /*"A combination generator" should "return Nil" in {
    assert(Diophantine.twoBoxCombs(null) == Nil)
    assert(Diophantine.twoBoxCombs(List()) == Nil)
  }*/

  /*"A combination generator" should "return all unique" in {
    //val list = List(PrimeFactor(3, 1), PrimeFactor(3, 1), PrimeFactor(3, 1), PrimeFactor(4, 1), PrimeFactor(5, 1), PrimeFactor(5, 1))
    val list = List(5, 4, 5, 5)
    val number = list.product
    println(s"Number for combination: $number")
    val combs = Diophantine.twoBoxCombs(list)
    val unique: ListBuffer[String] = ListBuffer[String]()
    combs.foreach(i => {
      val p1: Int = if (i._1.isEmpty) 1 else i._1.product
      val p2: Int = if (i._2.isEmpty) 1 else i._2.product
      val m = p1 * p2
        assert(number == m)
        val s = s"(${i._1.sortWith(_ < _).mkString("*")},${i._2.sortWith(_ < _).mkString("*")})"
        if (unique.contains(s)) {
          fail("Combination duplicate")
        } else {
          unique += s
        }
        println(s)
      })
    val pfList = toPrimeFactList(list)
    val count: Int = pfList.foldLeft(1)(
      (acc, item) => {
        if (item.power > 1) acc * (item.power + 1) else acc * 2
      })
    println(s"Count should be: $count")
    assert(combs.lengthCompare(count) == 0)
  }*/

  "Full generations" should "return result" in {
    val result = Diophantine.eval(64)
    result.foreach(i =>
      println(s"(${i._1}, ${i._2})")
    )
  }
}
