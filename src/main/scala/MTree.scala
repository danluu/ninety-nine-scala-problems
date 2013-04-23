package ninetynine {

  case class MTree[+T](value: T, children: List[MTree[T]]) {
    def this(value: T) = this(value, List())
    override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"
    def nodeCount: Int = children.foldLeft(1)((acc, child) => acc + child.nodeCount)
  }

  object MTree {
    def apply[T](value: T) = new MTree(value, List())
    // the given skeleton code for these problems has apply defined twice for some reason that I don't understand
    // def apply[T](value: T, children: List[MTree[T]]) = new MTree(value, children)
  }
}
