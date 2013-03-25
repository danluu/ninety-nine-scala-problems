package ninetynine

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class MTreeSuite extends FunSuite {
  test("P70 - nodeCount"){
    assert(MTree('a', List(MTree('f'))).nodeCount == 2)
    assert(MTree('a', List(MTree('f', List(MTree('g'))), MTree('c'), MTree('b', List(MTree('d'), MTree('e'))))).nodeCount == 7)
  }

}