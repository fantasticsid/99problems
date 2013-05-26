object Solution8 {
  def compress[T](lst: List[T]): List[T] = {
    lst match {
      case List() => List()
      case List(h) => List(h)
      case x::y::_ if x == y => compress(lst.tail)
      case _ => lst.head :: compress(lst.tail)
    }
  }
}
