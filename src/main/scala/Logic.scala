package ninetynine {
  object S99Logic {
    // written in a funny way to avoid using built in logical operators
    // would obviously be more efficient to define each operator in terms of ITE
    def nand(a: Boolean, b: Boolean) =
      if (a)
        if (b) false
        else true
      else true

    def not(a: Boolean) = nand(a, true)
    def and(a: Boolean, b: Boolean) = not(nand(a, b))
    def or(a: Boolean, b: Boolean) = nand(not(a), not(b))
    def nor(a: Boolean, b: Boolean) = not(or(a, b))
    def xor(a: Boolean, b: Boolean) = {
      val n = nand(a, b)
      nand(nand(n, a), nand(n, b))
    }
    def equ(a: Boolean, b: Boolean) = not(xor(a, b))
    def impl(a: Boolean, b: Boolean) = nand(a, not(b))
    // Note: P47 just moves all of P46 from the object into the class and creates an implicit conversion
    // I'm going to skip that since it's just busywork, and it doesn't seem to be used by later problems

    //TODO: memoize this
    def gray(i: Int): List[String] = {
      if (i == 1) List("0", "1")
      else gray(i - 1).map("0" ++ _) ++ gray(i - 1).reverse.map("1" ++ _)
    }

    // P50: I just did this exact problem for Odersky's Functional Programming in Scala class.
    // Unfortunately, they don't want solutions posted, so, I'm not including my solution in the repo on github
    // TODO: write a different implementation from scratch. Maybe try a solution using mutable state and a priority queue

  }
}
