object Solution5 {
  def reverse[T](lst: List[T]): List[T] = {
    if (lst.isEmpty) lst
    else reverse(lst.tail) ++ List(lst.head)
  }
}
