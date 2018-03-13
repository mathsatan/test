object Main extends App {
	def gcd(a: Int, b: Int): (Int, Int, Int) = {
		if (a == 0) return (b, 0, 1)
		val result = gcd(b%a, a)
		val d = result._1
		val x1 = result._2
		val y1 = result._3
		val x = y1 - (b / a) * x1
		val y = x1
		return (d, x, y)
	}
	def calc(a: Int, b: Int): Unit = {
		val res = gcd(a, b)
		println(s"$a*${res._2} + $b*${res._3} = ${res._1}")
	}
	calc(28, 123)
	calc(48, 14)
	calc(4, 32)
}