object Solution12 {
  def decode[T](lst: List[(Int, T)]): List[T] = {
    lst match {
      case List() => List()
      case (num: Int, elem) :: _ => repeat(num, elem) ::: decode(lst.tail) 
    }
  }
  
  def repeat[T](num: Int, elem: T): List[T] = {
    if (num == 0) List()
    else elem :: repeat(num - 1, elem)
  }
}