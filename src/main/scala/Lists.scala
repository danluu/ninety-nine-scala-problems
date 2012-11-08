package ninetynine

object Lists{ 
	def last[A] (xs: List[A]) = xs.last 
	def penultimate[A] (xs: List[A]) = xs.reverse.tail.head
	def nth[A] (i: Int, xs: List[A]) = xs(i)
	def length[A] (xs: List[A]) = xs.length
	def reverse[A] (xs: List[A]) = xs.reverse
	def isPalindrome[A] (xs: List[A]) = xs.equals(xs.reverse)
}
