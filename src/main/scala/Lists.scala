package ninetynine

object Lists {
  def last[A](xs: List[A]) = xs.last
  def penultimate[A](xs: List[A]) = xs.reverse.tail.head
  def nth[A](i: Int, xs: List[A]) = xs(i)
  def length[A](xs: List[A]) = xs.length
  def reverse[A](xs: List[A]) = xs.reverse
  def isPalindrome[A](xs: List[A]) = xs.equals(xs.reverse)
  def flatten[A](xs: List[A]): List[Any] = xs flatMap {
    case ys: List[_] => flatten(ys)
    case y => List(y)
  }
  //Note: could/should implement the following with dropWhile. Going to do the next problem with takeWhile
  //TODO: come back and do this with dropWhile
  def compress[A](xs: List[A]) = {
    def compress0[A](xs: List[A], acc: A): List[Any] = {
      if (xs.isEmpty) Nil
      else if (xs.head == acc) compress0(xs.tail, acc)
      else xs.head :: compress0(xs.tail, xs.head)
    }
    compress0(xs, Nil)
  }

  //TODO: should use break/span instead of takeWhile/dropWhile
  def pack[A](xs: List[A]): List[List[A]] = xs match {
    case Nil => Nil
    case ys => ys.takeWhile(_ == ys.head) :: pack(ys.dropWhile(_ == ys.head))
  }

  def encode[A](xs: List[A]) = pack(xs).map(x => (x.length, x.head))

  def encodeModified[A](xs: List[A]) = pack(xs).map(x =>
    if (x.length == 1) x.head
    else (x.length, x.head))

  def decode[A](xs: List[(Int, A)]) = xs flatMap { x => List.fill(x._1)(x._2) }
  //P13 is a duplicate of a previous problem. TODO: go back later and do it  

  def duplicate[A](xs: List[A]) = xs flatMap { x => List(x, x) }

  def duplicateN[A](i: Int, xs: List[A]) = xs flatMap { x => List.fill(i)(x) }

  //probably more efficient to do straight recursion with an accumulator. Could also zipWithIndex and then filter
  def drop[A](i: Int, xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case _ => xs.take(i - 1) ++ drop(i, xs.drop(i))
  }

  def split[A](i: Int, xs: List[A]): (List[A], List[A]) = xs.splitAt(i)

  def slice[A](i: Int, j: Int, xs: List[A]) = xs.slice(i, j)

  def rotate[A](i: Int, xs: List[A]) = {
    val rot = if (i > 0) i else xs.length + i
    xs.drop(rot) ++ xs.take(rot)
  }

  //should use splitAt instead of take/drop
  def removeAt[A](i: Int, xs: List[A]) = {
    val back = xs.drop(i)
    (xs.take(i) ++ back.tail, back.head)
  }

  def insertAt[A](x: A, i: Int, xs: List[A]) = xs.splitAt(i) match {
    case (ys, zs) => ys ::: x :: zs
  }

  def range(i: Int, j: Int) = List.range(i, j + 1)

  def randomSelect[A](i: Int, xs: List[A]) = {
    val r = new scala.util.Random
    def randomSelect0[A](i: Int, xs: List[A], r: util.Random): List[A] = {
      if (i <= 0) Nil
      else {
        val (rest, x) = removeAt(r.nextInt(xs.length), xs)
        x :: randomSelect0(i - 1, rest, r)
      }
    }
    randomSelect0(i, xs, r)
  }

  //Note: The solution on http://aperiodic.net/phil/scala/s-99/ doesn't match the problem description.
  def lotto(i: Int, j: Int) = randomSelect(i, List.range(1, j + 1))

  def randomPermute[A](xs: List[A]) = randomSelect(xs.length, xs)

  def combinations[A](i: Int, xs: List[A]): List[List[A]] = {
    xs.combinations(i).toList
  }

  def combinationsNoBuiltin[A](i: Int, xs: List[A]): List[List[A]] = {
    val s = xs.size
    if (i == 0) List(Nil)
    else if (i > s) Nil
    else if (i == s) List(xs)
    else combinationsNoBuiltin(i - 1, xs.tail).map(xs.head :: _) ::: combinations(i, xs.tail)
  }

  //TODO: can only think of a hack-y way to do P27 at the moment. Come back and do it later

  def lsort[A](xs: List[List[A]]): List[List[A]] = xs.sortBy(_.length)
}
