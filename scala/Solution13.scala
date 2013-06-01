object Solution13 {
  def encodeDirect[T](lst: List[T]): List[(Int, T)] = {
    lst match {
      case List() => List()
      case List(h) => List((1, h))
      case x :: y :: _ => {
        val encodedTail = encodeDirect(lst.tail)
        if (x == y) (encodedTail.head._1 + 1, x) :: encodedTail.tail
        else (1, x) :: encodedTail
      }
    }
  }
}