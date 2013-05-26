object Solution2 {
  def myLastButOne[T](lst: List[T]): T = {
    if (lst.length == 2) lst.head
    else myLastButOne(lst.tail)
  }
}
