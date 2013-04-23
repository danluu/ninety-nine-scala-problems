package ninetynine {
  class S99Int(val start: Int) {
    import S99Int._

    // note: no error handling here for < 0
    def isPrime: Boolean = primes.take(scala.math.sqrt(start).toInt + 1).contains(start)

    def isCoprimeTo(i: Int): Boolean = gcd(i, start) == 1

    // probably more efficient to filter and then take the length?
    def totient = Stream.range(1, start).map { x => if (x.isCoprimeTo(start)) 1 else 0 }.sum

    def primeFactors = {
      def primeFactors0(i: Int, j: Stream[Int], js: List[Int]): List[Int] =
        if (i % j.head == 0) primeFactors0(i / j.head, j, j.head :: js)
        else if (i > j.head) primeFactors0(i, j.tail, js)
        else js.reverse

      primeFactors0(start, primes, List())
    }

    def primeFactorMultiplicity = Lists.encode(primeFactors).map(_.swap)

    def phi = primeFactorMultiplicity.foldLeft(1) {
      (acc: Int, x: (Int, Int)) => (x._1 - 1) * scala.math.pow(x._1, (x._2 - 1)).toInt * acc
    }

    def goldbach: (Int, Int) = {
      val p1 = primes.find(x => (start - x).isPrime && x != (start - x)).takeWhile(_ < start).head
      (p1, start - p1)
    }
  }

  object S99Int {
    implicit def int2S99Int(i: Int): S99Int = new S99Int(i)

    // conveniently, just did this for Odersky's Functional Programming in Scala class
    def sieve(s: Stream[Int]): Stream[Int] = {
      s.head #:: sieve(s.tail.filter(x => x % s.head != 0))
    }
    val primes = sieve(Stream.from(2))

    def gcd(i: Int, j: Int): Int =
      if (j == 0) i
      else gcd(j, i % j)

    def listPrimesinRange(r: Range): List[Int] = primes.dropWhile(_ < r.head).takeWhile(_ <= r.last).toList

    def printGoldbachList(r: Range): Unit = r.foreach(x =>
      if (x % 2 != 0) Unit
      else {
        val ps = x.goldbach
        println(x.toString() + " = " + ps._1.toString + " + " + ps._2)
      })

    def printGoldbachListLimited(r: Range, i: Int): Unit = r.foreach(x =>
      if (x % 2 != 0) Unit
      else {
        val ps = x.goldbach
        if (scala.math.min(ps._1, ps._2) >= i) println(x.toString() + " = " + ps._1.toString + " + " + ps._2)
      })
  }
}
