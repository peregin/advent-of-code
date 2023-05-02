package leetcode.lt75

object CountPrimes extends App {

  // or sieve of eratosthenes
  def countPrimes(n: Int): Int = {
    def isPrime(x: Int): Boolean = !(2 to math.sqrt(x).toInt).exists(x % _ == 0)
    if (n > 1) (3 to n by 2).map(isPrime).count(_ == true) + 1 else 0
  }
  println(countPrimes(10))
}
