object Solution15 {
  def duplicateN[T](num: Int, lst: List[T]): List[T] = {
    lst match {
      case List() => List()
      case x :: xs => repeat(num, x) ::: duplicateN(num, xs)
    }
  }   

  def repeat[T](num: Int, elem: T): List[T] = {
    if (num == 0) List()
    else elem :: repeat(num - 1, elem)
  }
}