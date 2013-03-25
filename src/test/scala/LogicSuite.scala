package ninetynine

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class LogicSuite extends FunSuite {
  import S99Logic._
  
  //Note: P47 just moves all of P46 from the object into the class and creates an implicit conversion
  //I'm going to skip that since it's just busywork, and it doesn't seem to be used by later problems
  test("P46 - nand"){
    assert(nand(false, false) == true)
    assert(nand(false, true)  == true)
    assert(nand(true,  false) == true)
    assert(nand(true,  true)  == false)
  }
  
  test("P46 - not"){
	  assert(not(true) == false)
	  assert(not(false) == true)
  }

  test("P46 - and"){
    assert(and(false, false) == false)
    assert(and(false, true)  == false)
    assert(and(true,  false) == false)
    assert(and(true,  true)  == true)
  }
  
  test("P46 - or"){
    assert(or(false, false) == false)
    assert(or(false, true)  == true)
    assert(or(true,  false) == true)
    assert(or(true,  true)  == true)    
  }
  
  test("P46 - nor"){
    assert(nor(false, false) == true)
    assert(nor(false, true)  == false)
    assert(nor(true,  false) == false)
    assert(nor(true,  true)  == false)    
  }  
  
  test("P46 - xor"){
    assert(xor(false, false) == false)
    assert(xor(false, true)  == true)
    assert(xor(true,  false) == true)
    assert(xor(true,  true)  == false)    
  }
  
  test("P46 - equ"){
    assert(equ(false, false) == true)
    assert(equ(false, true)  == false)
    assert(equ(true,  false) == false)
    assert(equ(true,  true)  == true)    
  }
  
  test("P46 - impl"){
    assert(impl(false, false) == true)
    assert(impl(false, true)  == true)
    assert(impl(true,  false) == false)
    assert(impl(true,  true)  == true)    
  }  
  
  test("P49 - gray"){
    assert(gray(1) == List("0", "1"))
    assert(gray(2) == List("00", "01", "11", "10"))
    assert(gray(3) == List("000", "001", "011", "010", "110", "111", "101", "100"))
  }
  
}