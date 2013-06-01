object Solution16 {
  def drop[T](num: Int, lst: List[T]): List[T] = {
    if (lst.length < num) lst
    else {
      val headBatch = take(num - 1, lst)
      val tailBatch = dropFor(num, lst)
      headBatch ::: drop(num, tailBatch)
    } 
  }
  
  def take[T](num: Int, lst: List[T]): List[T] = {
    if (num == 0) List()
    else lst.head :: take(num - 1, lst.tail)
  }
  
  def dropFor[T](num: Int, lst: List[T]): List[T] = {
    if (num == 0) lst
    else dropFor(num - 1, lst.tail)
  }
}