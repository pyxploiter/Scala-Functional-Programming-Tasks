package main
import scala.collection.mutable

object main {
  def main(args: Array[String]) {
	  val numList: List[Int] = List(9,8,7,6,5,4,3,2,1)
	  println(lastNth(8,numList))
	  println(isPrime(15))
	  println(isPrime(23))
	  println(areCoprime(2,5))
	  println(areCoprime(2,6))
  }
  
  //finding last nth element in list
  def lastNth(n: Int, list: List[Int]): Int = {
    if (n == list.length-(list.length-n)){
      return n
    }
    else{
      lastNth(n, list.slice(0, list.length-n))
    }
  }
  
  def lastNthRecursive(n:Int, list: List[Int]): Int = { 
    return n
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
  def greatestCommonDivisor(i: Int, j: Int): Int =
    if (j == 0) i
    else greatestCommonDivisor(j, i % j)
  
  //checking if two positive integers are coprime
  def areCoprime(n1: Int, n2: Int): Boolean = (greatestCommonDivisor(n1,n2) == 1)
  
  //XOR operation for two logical expressions
  
  //AND operation for two logical expressions
  
  //NAND operation for two logical expressions
}