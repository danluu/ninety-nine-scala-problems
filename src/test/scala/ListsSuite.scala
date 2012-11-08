package ninetynine

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class ListsSuite extends FunSuite {
  
  import ninetynine.Lists._

  test("P01 -- last element of list") {
    assert(last(List(1,2,3)) == 3)
    assert(last(List('x')) == 'x')
    assert(last(List(1, 1, 2, 3, 5, 8)) == 8)
  }

  test("P02 -- penultimate element of list") {
   assert(penultimate(List(1, 1, 2, 3, 5, 8)) == 5)
  }
  
  test("P03 -- nth element of list") {
   assert(nth(0, List(1, 1, 2, 3, 5, 8)) == 1)
   assert(nth(1, List(1, 1, 2, 3, 5, 8)) == 1)
   assert(nth(2, List(1, 1, 2, 3, 5, 8)) == 2)
  }
  
  test("P04 -- length of a list") {
   assert(length(List(1, 1, 2, 3, 5, 8)) == 6)
  }
 
  test("P05 -- reverse a list") {
   assert(reverse(List(1, 1, 2, 3, 5, 8)).equals(List(8, 5, 3, 2, 1, 1)))
  }  
  
  test("P06 -- isPalindrome ") {
   assert(isPalindrome(List(1, 2, 3, 2, 1)) == true)
   assert(isPalindrome(List(1, 2, 3, 2, 2)) == false)
   assert(isPalindrome(List(1, 2, 2, 1)) == true)
   assert(isPalindrome(List(1, 2, 1, 1)) == false)
  }    
}
