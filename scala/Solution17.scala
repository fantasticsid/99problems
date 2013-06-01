object Solution17 {
  def split[T](num: Int, lst: List[T]): (List[T], List[T]) = {
    val splitTuple = zipperSplit(num, lst)
    (splitTuple._1.reverse, splitTuple._2)
  }
  
  def zipperSplit[T](num: Int, lst: List[T]): (List[T], List[T]) = {
    def iterZipperSplit[T](num: Int, xs: List[T], ys: List[T]): (List[T], List[T]) = {
      if (num == 0) (xs, ys)
      else iterZipperSplit(num - 1, ys.head :: xs, ys.tail)
    }
    iterZipperSplit(num, List(), lst)
  }
}