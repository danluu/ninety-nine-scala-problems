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
  
  test("P56 - isSymmetric"){
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
  
  test("P57 - (unbalanced) binary search tree"){
    assert(End.addValue(2).addValue(3).addValue(0) == Node(2,Node(0),Node(3)))
    assert(Tree.fromList(List(3, 2, 5, 7, 1)) == Node(3,Node(2,Node(1),End),Node(5,End,Node(7))))
  }
  
  test("P56 + P57"){
    assert(Tree.fromList(List(5, 3, 18, 1, 4, 12, 21)).isSymmetric == true)
    assert(Tree.fromList(List(3, 2, 5, 7, 4)).isSymmetric == false)
  }
  
  test("P58 - symmetric completely balanced binary trees"){
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
  
  test("P59 - all height balanced trees, given height"){
    assert(Tree.hbalTrees(1, 'a) == List(Node('a)))
    assert(Tree.hbalTrees(2, 'c) == List(Node('c,Node('c),Node('c)), Node('c,Node('c),End), Node('c,End,Node('c))))
    assert(Tree.hbalTrees(3, 'd).length == 15)
    assert(Tree.hbalTrees(4, 'e).length == 315)
  }
  
  test("P60 - number of height balanced trees, given number of nodes"){
    //helper functions
    assert(Tree.minHbalNodes(3) == 4)
    assert(Tree.maxHbalHeight(4) == 3)
    assert(Tree.minHbalHeight(1) == 1)
    assert(Tree.minHbalHeight(2) == 2)
    assert(Tree.minHbalHeight(3) == 2)
    assert(Tree.minHbalHeight(4) == 3)
    assert(Tree.minHbalHeight(7) == 3)
    assert(Tree.minHbalHeight(8) == 4)
    assert(Node('a).size == 1)
    assert(Node('b,Node('b),End).size == 2)
    assert(Node('c,Node('c),Node('c)).size == 3)
    
    assert(Tree.hbalTreesWithNodes(1, 'a) == List(Node('a)))
    
    val hbalTrees2 = Tree.hbalTreesWithNodes(2, 'a)
    assert(hbalTrees2.length == 2)
    assert(hbalTrees2.toSet == Set(Node('a,Node('a),End), Node('a,End,Node('a))))
    
    assert(Tree.hbalTreesWithNodes(3, 'a) == List(Node('a,Node('a),Node('a))))
    
    val hbalTrees4 = Tree.hbalTreesWithNodes(4, 'd)
    assert(hbalTrees4.length == 4)
    assert(hbalTrees4.toSet == Set(Node('d,Node('d,Node('d),End),Node('d)), Node('d,Node('d),Node('d,Node('d),End)), Node('d,Node('d,End,Node('d)) ,Node('d)), Node('d,Node('d),Node('d,End,Node('d)))))
    
    //TODO: write tests for higher numbers of nodes. It should be easy to figure how many trees satisfy the criteria
  }
  
  test("P61 - leafCount"){
    assert(Node('x', Node('x'), End).leafCount == 1)
    assert(Node('x', Node('x'), Node('x')).leafCount == 2)
  }
  
  test("P61A - leafList"){
    assert((Node('a, Node('b), Node('c, Node('d), Node('e))).leafList == List('b, 'd, 'e)))
  }
  
  test("P62 - internalList"){
    assert(Node('a, Node('b), Node('c, Node('d), Node('e))).internalList == List('a, 'c))
  }
  
  test("P62B - atLevel"){
    assert(Node('a, Node('b), Node('c, Node('d), Node('e))).atLevel(1) == List('a))
    assert(Node('a, Node('b), Node('c, Node('d), Node('e))).atLevel(2) == List('b, 'c))
    assert(Node('a, Node('b), Node('c, Node('d), Node('e))).atLevel(3) == List('d, 'e))
  }
  
  test("P63 - completeBinaryTree"){
    assert(Tree.completeBinaryTree(6, 'x) == Node('x,Node('x,Node('x),Node('x)),Node('x,Node('x),End)))
  }
  
  //TODO: go back and do the binary tree layout questions
  // 64 is done and needs a test. Skipping 65 and 66
  
  test("P67 - toString alternative and fromString"){
    assert(Node('a', Node('b', Node('d'), Node('e')), Node('c', End, Node('f', Node('g'), End))).toString67 == "a(b(d,e),c(,f(g,)))")
    assert(Tree.fromString("a(b(d,e),c(,f(g,)))") == 
      Node("a",Node("b",Node("d"),Node("e")),Node("c",End,Node("f",Node("g"),End))))
  }
  
  test("P68 - preorder and inorder traversals"){
    assert(Tree.fromString("a(b(d,e),c(,f(g,)))").preOrder == List("a", "b", "d", "e", "c", "f", "g")) 
    assert(Tree.fromString("a(b(d,e),c(,f(g,)))").inOrder ==  List("d", "b", "e", "a", "c", "g", "f"))
  }
}
