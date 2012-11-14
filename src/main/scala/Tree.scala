package ninetynine {

  sealed abstract class Tree[+T]{
    def isMirrorOf[U](t: Tree[U]): Boolean
    def isSymmetric: Boolean
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    def leaves = "," + left.toString + "," + right.toString
    def tailString = if (leaves == ",End,End") "" 
    else leaves
    override def toString = "Node(" + value.toString + tailString + ")"
    override def isMirrorOf[U](t: Tree[U]) = {
      def isMirrorOf0[U](t0: Tree[U]) = t0 match {
        case n0: Node[U] => left.isMirrorOf(n0.left) && right.isMirrorOf(n0.right)  
        case _ => false
      }
      t match {
        case n: Node[U] => left.isMirrorOf(n.right) && right.isMirrorOf(n.left)
        case _ => false
      }
    }
    override def isSymmetric = left.isMirrorOf(right) 
    //TODO: think about isSymmetric and write more tests. Something feels off
    
    override def addValue[U >: T <% Ordered[U]](x: U) = 
      if (x < value) Node(value,left.addValue(x),right)
      else Node(value, left, right.addValue(x))
  }

  case object End extends Tree[Nothing] {
    override def toString = "End"
    override def isMirrorOf[U](t: Tree[U]) = t == End
    override def isSymmetric = true
    override def addValue[U >: Nothing <% Ordered[U]](x: U) = Node(x)
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }
  
  object Tree{   
	def cBalanced[T](i: Int, x: T): List[Tree[T]] = {
	  if (i <= 0) List(End)
	  // else if (i % 2 == 1) Node(x,cBalanced(i/2,x)),cBalanced(i/2,x)) want to write this, but with flatMap
	  // because cBalanced returns a list and not a Node. Alternately, could use a for comp
	  else if (i % 2 == 1){ 
	    val t = cBalanced(i/2, x)
	    t.flatMap(l => t.map(r => Node(x,l,r)))
	  }else{ // i % 2 == 0
	    val t1 = cBalanced(i/2,x)
	    val t2 = cBalanced(i/2-1,x)
	    t1.flatMap(big => t2.flatMap(small => List(Node(x, big, small), Node(x, small, big))))
	  }
	}
	
	def fromList[T <% Ordered[T]](l: List[T]): Tree[T] = l.foldLeft(End: Tree[T])((t,x) => t.addValue(x))
	
	def symmetricBalancedTrees[T](i: Int, x: T): List[Tree[T]] = cBalanced(i,x).filter(_.isSymmetric)
	
  }
  
}