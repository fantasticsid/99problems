object Solution4 {
  def length[T](lst: List[T]): Int = {
    if (lst.isEmpty) 0
    else 1 + length(lst.tail)
  }
}
