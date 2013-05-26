object Solution1 {
  def mylast[T](lst: List[T]): T = {
    if (lst.length == 1) lst.head
    else mylast(lst.tail)
  }
}
