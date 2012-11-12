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
  
  test("P07 -- recursive flatten"){
    assert(flatten(List(List(1, 1), 2, List(3, List(5, 8)))) == List(1, 1, 2, 3, 5, 8))
  }
  
  test("P08 -- compress"){
    assert(compress(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List('a, 'b, 'c, 'a, 'd, 'e))
  }
  
  test("P09 -- pack"){
    assert(pack(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List(List('a, 'a, 'a, 'a), List('b), List('c, 'c), List('a, 'a), List('d), List('e, 'e, 'e, 'e)))
  }  
  
  test("P10 -- encode"){
	assert(encode(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), (1,'b), (2,'c), (2,'a), (1,'d), (4,'e)))
  }
  
  test("P11 -- encodeModified"){
	assert(encodeModified(List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)) == List((4,'a), 'b, (2,'c), (2,'a), 'd, (4,'e)))
  }
  
  test("P12 -- decode"){
    assert(decode(List((4, 'a), (1, 'b), (2, 'c), (2, 'a), (1, 'd), (4, 'e))) == List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e))
  }
  
  //P13 is a duplicate of a previous problem. TODO: go back later and do it
  
  test("P14 -- duplicate"){
   assert(duplicate(List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'b, 'b, 'c, 'c, 'c, 'c, 'd, 'd))
  }
  
  test("P15 -- duplicateN"){
    assert(duplicateN(3, List('a, 'b, 'c, 'c, 'd)) == List('a, 'a, 'a, 'b, 'b, 'b, 'c, 'c, 'c, 'c, 'c, 'c, 'd, 'd, 'd))
  }
  
  test("P16 -- drop"){
    assert(drop(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('a, 'b, 'd, 'e, 'g, 'h, 'j, 'k))
  }

  test("P17 -- split"){
     assert(split(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == (List('a, 'b, 'c),List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k)))
  }
  
  test("P18 -- slice"){
    assert(slice(3, 7, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g))
  }
  
  test("P19 -- rotate"){
    assert(rotate(3, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('d, 'e, 'f, 'g, 'h, 'i, 'j, 'k, 'a, 'b, 'c))
    assert(rotate(-2, List('a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i, 'j, 'k)) == List('j, 'k, 'a, 'b, 'c, 'd, 'e, 'f, 'g, 'h, 'i))
  }
  
  test("P20 -- removeAt"){
    assert(removeAt(1, List('a, 'b, 'c, 'd)) == (List('a, 'c, 'd),'b))
  }

  test("P21 -- insertAt"){
    assert(insertAt('new, 1, List('a, 'b, 'c, 'd)) == List('a, 'new, 'b, 'c, 'd))
  }
  
  test("P22 -- range"){
     assert(range(4, 9) == List(4, 5, 6, 7, 8, 9))
  }
  
  test("P23 -- randomSelect"){
    //TODO: come up with a method of testing random selection
    //Note that the current example on http://aperiodic.net/phil/scala/s-99/ is wrong
  }
  
  test("P24 -- lotto"){
    //TODO: come up with a method of testing random selection 
  }
  
  test("P25 -- randomPermute"){
    //TODO: come up with a method of testing random selection 
  }
  
  test("P26 -- combinations"){
    val c1 = combinations(1, List('a,'b,'c))
    val a1 = List(List('a), List('b), List('c))
    assert(c1.length == a1.length && c1.toSet == a1.toSet)
    
    val c2 = combinations(2, List('a,'b,'c))
    val a2 = List(List('a, 'b), List('a, 'c), List('b, 'c))
    assert(c2.length == a2.length && c2.toSet == a2.toSet)
    
    val c3 = combinations(3, List('a, 'b, 'c, 'd, 'e, 'f))
    val a3 = List(List('a, 'b, 'c), List('a, 'b, 'd), List('a,
                                                  'b, 'e), List('a, 'b, 'f), List('a, 'c, 'd), List('a, 'c, 'e), List('a, 'c,
                                                  'f), List('a, 'd, 'e), List('a, 'd, 'f), List('a, 'e, 'f), List('b, 'c, 'd)
                                                  , List('b, 'c, 'e), List('b, 'c, 'f), List('b, 'd, 'e), List('b, 'd, 'f), List
                                                   ('b, 'e, 'f), List('c, 'd, 'e), List('c, 'd, 'f), List('c, 'e, 'f), List(
                                                   'd, 'e, 'f))
    assert(c3.length == a3.length && c3.toSet == a3.toSet)                                                                                               
  }
  
  test("P27 -- disjoint subets"){
    assert(1 == 0) //TODO: can't think of a non-hacky way to do this atm.
  }
  
  test("P28 -- sort list based on length"){
     assert(lsort(List(List('a, 'b, 'c), List('d, 'e), List('f, 'g, 'h), List('d, 'e), List('i, 'j, 'k, 'l), List('m, 'n), List('o))) == List(List('o), List('d, 'e), List('d, 'e), List('m, 'n), List('a, 'b, 'c), List('f, 'g, 'h), List('i, 'j, 'k, 'l)))
  }
}