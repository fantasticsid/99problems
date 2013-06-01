object Solution6 {
  def reverse[T](lst: List[T]): List[T] = {
    if (lst.isEmpty) lst
    else reverse(lst.tail) ++ List(lst.head)
  }

  def isPalindrome[T](lst: List[T]): Boolean = {
    reverse(lst) == lst
  }
}
