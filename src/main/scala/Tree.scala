package ninetynine {
  import scala.util.parsing.combinator._

  sealed abstract class Tree[+T] {
    def isMirrorOf[U](t: Tree[U]): Boolean
    def isSymmetric: Boolean
    def addValue[U >: T <% Ordered[U]](x: U): Tree[U]
    def size: Int
    def leafCount: Int
    def leafList: List[T]
    def internalList: List[T]
    def atLevel(level: Int): List[T]
    //    def layoutBinaryTree: Tree[T] = layoutBinaryTreeHelper(1, 1)._1
    //    def layoutBinaryTreeHelper(x: Int, y: Int): (Tree[T], Int)
    def toString67: String
    def preOrder: List[T]
    def inOrder: List[T]
  }

  case class Node[+T](value: T, left: Tree[T], right: Tree[T]) extends Tree[T] {
    def leaves = "," + left.toString + "," + right.toString
    def tailString = if (leaves == ",End,End") ""
    else leaves
    override def toString = "Node(" + value.toString + tailString + ")"
    override def toString67 = (left, right) match {
      case (End, End) => value.toString
      case _ => value.toString + "(" + left.toString67 + "," + right.toString67 + ")"
    }
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
      if (x < value) Node(value, left.addValue(x), right)
      else Node(value, left, right.addValue(x))

    override def size = 1 + left.size + right.size

    //would be more efficient to do this directly
    override def leafCount = leafList.length
    override def leafList = (left, right) match {
      case (End, End) => List(value)
      case _ => left.leafList ++ right.leafList
    }

    override def internalList = (left, right) match {
      case (End, End) => Nil
      case _ => value :: left.internalList ++ right.internalList
    }

    override def atLevel(level: Int) =
      if (level <= 0) Nil
      else if (level == 1) List(value)
      else left.atLevel(level - 1) ++ right.atLevel(level - 1)

    override def preOrder = value :: left.preOrder ::: right.preOrder
    override def inOrder = left.inOrder ::: value :: right.inOrder
  }

  /* case class inheritance is illegal in 2.10, but the problem actually specifies this class structure. Use scala 2.9.1 if you want to solve this problem */
  /*
    
    //x is the inorder traversal number, so it must be incremented each time we've finished a left traversal
    //and are about to start a right traversal
    override def layoutBinaryTreeHelper(x: Int, y: Int): (Tree[T], Int) = {
      val (leftHelper, leftX) = left.layoutBinaryTreeHelper(x, 1+y)
      val (rightHelper, rightX) = right.layoutBinaryTreeHelper(leftX+1, 1+y)
      (PositionedNode(value, leftHelper, rightHelper, leftX, y),rightX)
      }
    
  }
  
  case class PositionedNode[T](override val value: T, override val left: Tree[T], override val right: Tree[T], x: Int, y: Int) extends 
    Node(value, left, right){
      override def toString =  "T[" + x.toString + "," + y.toString + "](" + value.toString + " " + left.toString + " " + right.toString + ")"
   */

  case object End extends Tree[Nothing] {
    override def toString = "End"
    override def isMirrorOf[U](t: Tree[U]) = t == End
    override def isSymmetric = true
    override def addValue[U >: Nothing <% Ordered[U]](x: U) = Node(x)
    override def size = 0
    override def leafCount = 0
    override def leafList = Nil
    override def internalList = Nil
    override def atLevel(level: Int) = Nil
    //    override def layoutBinaryTreeHelper(x: Int, y: Int): (Tree[Nothing], Int) =
    //      (End, x)
    override def toString67 = ""
    override def preOrder = Nil
    override def inOrder = Nil
  }

  object Node {
    def apply[T](value: T): Node[T] = Node(value, End, End)
  }

  object TreeParser extends JavaTokenParsers {
    def leaf: Parser[Node[String]] = ident ^^ (Node(_))
    def child: Parser[Tree[String]] = node | leaf | "" ^^ (_ => End)
    def node: Parser[Tree[String]] = ident ~ ("(" ~> child <~ ",") ~ (child <~ ")") ^^
      { case v ~ l ~ r => Node(v, l, r) } |
      leaf
  }

  object Tree {
    def cBalanced[T](i: Int, x: T): List[Tree[T]] = {
      if (i <= 0) List(End)
      // else if (i % 2 == 1) Node(x,cBalanced(i/2,x)),cBalanced(i/2,x)) want to write this, but with flatMap
      // because cBalanced returns a list and not a Node. Alternately, could use a for comp
      else if (i % 2 == 1) {
        val t = cBalanced(i / 2, x)
        t.flatMap(l => t.map(r => Node(x, l, r)))
      } else { // i % 2 == 0
        val t1 = cBalanced(i / 2, x)
        val t2 = cBalanced(i / 2 - 1, x)
        t1.flatMap(big => t2.flatMap(small => List(Node(x, big, small), Node(x, small, big))))
      }
    }

    def fromList[T <% Ordered[T]](l: List[T]): Tree[T] = l.foldLeft(End: Tree[T])((t, x) => t.addValue(x))

    def symmetricBalancedTrees[T](i: Int, x: T): List[Tree[T]] = cBalanced(i, x).filter(_.isSymmetric)

    def hbalTrees[T](i: Int, x: T): List[Tree[T]] =
      if (i <= 0) List(End)
      else if (i == 1) List(Node(x))
      else {
        val t1 = hbalTrees(i - 1, x)
        val t2 = hbalTrees(i - 2, x)
        //we want full trees (x, tall, tall), as well trees with a hole(x, tall, short)
        t1.flatMap(tall => t1.flatMap(tall2 => List(Node(x, tall, tall2)))) ++
          t2.flatMap(short => t1.flatMap(tall => List(Node(x, tall, short), Node(x, short, tall))))
      }

    def minHbalNodes(i: Int): Int =
      if (i <= 0) 0
      else if (i == 1) 1
      else minHbalNodes(i - 1) + minHbalNodes(i - 2) + 1

    def minHbalHeight(i: Int): Int =
      if (i <= 0) 0
      else 1 + minHbalHeight(i / 2)

    def maxHbalNodes(i: Int): Int =
      scala.math.pow(2.0, i.toDouble).toInt - 1

    def maxHbalHeight(i: Int): Int = {
      def maxHbalHeight0(height: Int): Int =
        if (minHbalNodes(height) <= i) maxHbalHeight0(height + 1)
        else height - 1
      maxHbalHeight0(1)
    }

    def hbalTreesWithNodes[T](nodes: Int, x: T): List[Tree[T]] =
      (minHbalHeight(nodes) to maxHbalHeight(nodes)).flatMap(hbalTrees(_, x)).filter(_.size == nodes).toList

    //TODO: understand why we need to pass x to completeBinaryTree0 to avoid a type error  
    def completeBinaryTree[T](nodes: Int, x: T): Tree[T] = {
      def completeBinaryTree0[T](addr: Int, x: T): Tree[T] =
        if (addr > nodes) End
        else Node(x, completeBinaryTree0(2 * addr, x), completeBinaryTree0(1 + 2 * addr, x))
      completeBinaryTree0(1, x)
    }

    //Note that this differs from the example in P67, which produces a Tree[Char]. We output a Tree[String], to allow more flexible naming
    def fromString(s: String) = TreeParser.parseAll(TreeParser.node, s).get

  }

}
