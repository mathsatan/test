package com.kruchkov.numbers

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

object Primes {
  def isPrime(_x: Long): Boolean = {
    @tailrec
    def isPositivePrime(n: Long, i: Long): Boolean = {
      if (n % i == 0) return false
      if (i - 1 < 2) return true
      isPositivePrime(n, i - 1)
    }
    val x: Long = Math.abs(_x)
    if (x < 2) return false
    if (x == 2) return true
    val s = Math.ceil(Math.sqrt(x)).toLong
    isPositivePrime(x, s)
  }

  def factorize(x: Int): Try[List[Int]] = {
    @tailrec
    def f(x: Int, a: Int = 2, list: List[Int] = Nil): List[Int] = a * a > x match {
      case false if x % a == 0 => f(x / a, a    , a :: list)
      case false               => f(x    , a + 1, list)
      case true                => x :: list
    }
    if (x < 2)
      Failure(new IllegalArgumentException("Argument is below 2"))
    else Success(f(x))
  }

  def gcd(a: Int, b: Int): (Int, Int, Int) = {
    if (a == 0) return (b, 0, 1)
    val result = gcd(b%a, a)
    val d = result._1
    val x1 = result._2
    val y1 = result._3
    val x = y1 - (b / a) * x1
    val y = x1
    (d, x, y)
  }

  def calc(a: Int, b: Int): Unit = {
    val res = gcd(a, b)
    println(s"$a*${res._2} + $b*${res._3} = ${res._1}")
  }

}
