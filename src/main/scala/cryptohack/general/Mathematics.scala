package cryptohack.general

import scala.annotation.tailrec

object Mathematics {
  //Mathematics.gcd(66528,52920)
  @tailrec
  def gcd(a: Int, b: Int): Int = {
    val max = Math.max(a, b)
    val min = Math.min(a, b)
    if (max % min == 0) min
    else gcd(max % min, min)
  }

  // Mathematics.extendedGCD(26513,32321)
  @tailrec
  def extendedGCD(oldR: Int, r: Int, oldS: Int = 1, s: Int = 0, oldT: Int = 0, t: Int = 1): (Int, Int) =
    if (r == 0) {
      (oldS, oldT)
    } else {
      val quotient = oldR / r
      extendedGCD(r, oldR - quotient * r, s, oldS - quotient * s, t, oldT - quotient * t)
    }

  def modular1: Long = {
    val a = 11             % 6
    val b = 8146798528947L % 17
    Math.min(a, b)
  }

  def modular2(a: Int, pow: Int, mod: Int): Long = {
    if (pow == mod) a
    else if (mod - pow == 1) 1
    else Math.pow(a, pow).toLong % mod
  }

  def modularInverting(a: Int, mod: Int): Int = {
    BigInt(a).modInverse(BigInt(mod)).toInt
  }



}
