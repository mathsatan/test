package com.kruchkov.numbers

object PrimeFactor {
  def apply(n: Int, power: Int = 0) = new PrimeFactor(n, power)

}

class PrimeFactor(val n: Int, var power: Int = 0) {
  override def toString(): String = if (power > 1) s"$n^$power" else n.toString

  def incPower = power = power + 1
  def getValue: Int = Math.pow(n, power).toInt

  def *(other: PrimeFactor): PrimeFactor = {
    val value: Int = (Math.pow(this.n, this.power) * Math.pow(other.n, other.power)).toInt
    PrimeFactor(value, 1)
  }

  def toInt: Int = Math.pow(this.n, this.power).toInt

}
