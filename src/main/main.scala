package main
import scala.collection.immutable.List
import scala.util.Random

object main {
  def main(args: Array[String]) {
	  val numList: List[Int] = List(9,11,7,22,5,4,3,2,1)
    
    //testing all functions
	  println(lastNth(8,numList))
	  println(lastNthRecursive(6, numList))
	  
	  println(isPrime(15))
	  println(isPrime(23))
	  
	  println(areCoprime(2,5))
	  println(areCoprime(2,6))
	  
	  println("XOR Table:-");
	  println(XOR(true,true))
	  println(XOR(false,true))
	  println(XOR(true,false))
	  println(XOR(false,false))
	  
	  println("AND Table:-");
	  println(AND(true,true))
	  println(AND(false,true))
	  println(AND(true,false))
	  println(AND(false,false))
	  
	  println("NAND Table:-");
	  println(NAND(true,true))
	  println(NAND(false,true))
	  println(NAND(true,false))
	  println(NAND(false,false))
	  
	  //generated 5 random numbers
	  val num1 = range.positiveInt(1)
    val num2 = range.positiveInt(2)
    val num3 = range.positiveInt(3)
    val num4 = range.positiveInt(4)
    val num5 = range.positiveInt(5)
  }
  
  //finding last nth element in list
  def lastNth(n: Int, list: List[Int]): Int = list(list.length-n)
  
  //finding last nth element using tail-recursive
  def lastNthRecursive(n:Int, list: List[Int]): Int = { 
    if (n == 0){
      return list.last
    }
    else{
      lastNth(n-1, list.take(list.length-1))
    }
  }
  
  //function for checking if the number is prime
  def isPrime(n:Int): Boolean = {
     if (n < 2)
      throw new IllegalArgumentException("Prime numbers can only be greater than equal to 2")
     else
      findIfPrime(n, n/2)
  }
  
  //finding prime number by recursion
  def findIfPrime(n1: Int, n2: Int): Boolean = {
    if (n2 == 1) true
    else {
      if (n1 % n2 == 0) false
      else findIfPrime(n1,n2-1)
    }
  }
  
  //finding the greatest common divisor
  def greatestCommonDivisor(n1: Int, n2: Int): Int =
    if (n2 == 0) n1
    else greatestCommonDivisor(n2, n1 % n2)
  
  //checking if two positive integers are coprime
  def areCoprime(n1: Int, n2: Int): Boolean = (greatestCommonDivisor(n1,n2) == 1)
  
  //XOR operation for two logical expressions
  def XOR(n1: Boolean, n2: Boolean): Boolean = {
      if (n1 == n2) false
      else true
    }
  
  //AND operation for two logical expressions
  def AND(n1: Boolean, n2: Boolean): Boolean = {
    if (n1 == true && n2 == true) true
    else false
  }
  
  //NAND operation for two logical expressions
  def NAND(n1: Boolean, n2: Boolean): Boolean = !AND(n1,n2)
  
  //object for generating random numbers
  object range {
    trait RNG {
      def nextInt: (Int, RNG)
    }

    case class Simple(seed: Long) extends RNG {
      def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = Simple(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
      }
    }

    def positiveInt(seed:Int): (Int, RNG) = {
      val rng: RNG = Simple(seed)
      val (n, newRng) = rng.nextInt
      (n%100+1, newRng)
    }
  }
}

