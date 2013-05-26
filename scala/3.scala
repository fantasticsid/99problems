object Solution3 {
  def kth[T](n: Int, lst: List[T]): T = {
    if (n == 0) lst.head
    else kth(n-1, lst.tail)
  }
}
