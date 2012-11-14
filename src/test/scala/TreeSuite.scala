package ninetynine

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TreeSuite extends FunSuite {
  test("P55 - completely balanced binary trees"){
    assert(Tree.cBalanced(1,'a) == List(Node('a)))
    assert(Tree.cBalanced(2,'b) == List(Node('b,Node('b),End), Node('b,End,Node('b))))
    assert(Tree.cBalanced(3,'c) == List(Node('c,Node('c),Node('c))))
    
  }
  
  test("P66 - isSymmetric"){
    assert(Node('a', Node('b'), Node('c')).isSymmetric == true)
    assert(Node('a', End, Node('c')).isSymmetric == false)
    assert(Node('a', Node('b'), End).isSymmetric == false)
    assert(Node('a').isSymmetric == true)
    
    val rgt = Node('a,End,Node('b,End,Node('c)))
    val lft = Node('a,Node('b,Node('c),End),End)
    assert(Node('x,lft,rgt).isSymmetric == true)
    assert(Node('x,rgt,lft).isSymmetric == true)
    assert(Node('x,rgt,rgt).isSymmetric == false)
    assert(Node('x,lft,lft).isSymmetric == false)
  }
  
  test("P67 - (unbalanced) binary search tree"){
    assert(End.addValue(2).addValue(3).addValue(0) == Node(2,Node(0),Node(3)))
    assert(Tree.fromList(List(3, 2, 5, 7, 1)) == Node(3,Node(2,Node(1),End),Node(5,End,Node(7))))
  }
  
  test("P66 + P67"){
    assert(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric == true)
    assert(Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric == false)
  }
  
  test("P68 - symmetric completely balanced binary trees"){
    val l = Tree.symmetricBalancedTrees(5, 'x)
    assert(l.length == 2)
    assert(l.toSet == Set(Node('x,Node('x,Node('x),End),Node('x,End,Node('x))), Node('x,Node('x,End,Node('x)),Node('x,Node('x),End))))
    
    val evenSymmetricTrees = for{
      i <- (1 to 10) //for any even number except 0, there should be no possible tree. 'End', by itself, works for 0
      if i % 2 == 0
      } yield Tree.symmetricBalancedTrees(i, 'x)
      
     assert(evenSymmetricTrees.toList.flatten.length == 0)
     //for the odd numbers, the pattern sequence should be: 1, 1, 2, 1, 4, 4, 4, 1, 8, 16, 32, 16, 32, 16, 8, 1, 16, 64, 256, 256, 1024
     // i.e., a(2n) = 2a(n)a(n-1), a(2n+1) = a(n)a(n). Generating all cBalanced trees with our method makes this too slow to
     // test for large numbers, though
  }
  
  
}