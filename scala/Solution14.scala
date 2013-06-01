object Solution14 {
  def duplicate[T](lst: List[T]): List[T] = {
    lst match {
      case List() => List()
      case x :: xs => x :: x :: duplicate(xs)
    }
  }
}