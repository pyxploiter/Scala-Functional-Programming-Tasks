object sieve {
  def main(args: Array[String]) {
    print("Enter a number: ")  
      val n: Int = scala.io.StdIn.readInt()
      if (n < 2) throw new IllegalArgumentException()
      val prime_list = sieve_prime(n)
      prime_list.map(f => print(f+" "));
  }
  
  def sieve_prime(n: Int): List[Int] = {
    //odd numbers from 3 to square-root(n)
      val oddsUptoSqrtN = Stream.from(3,2).takeWhile(_ <= Math.sqrt(n).toInt).toList
      //composites of odds numbers we got above
      val composites = oddsUptoSqrtN.flatMap(f => Stream.from(f*f,2*f).takeWhile(_ <= n).toList)
      //calculating list of prime numbers upto N
      return 2::Stream.from(3,2).takeWhile(_ <= n).diff(composites).toList
  }
}
