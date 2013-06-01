object Solution11 {
  def encodeModified[T](lst: List[T]): List[Any] = {
    lst match {
      case List() => lst
      case List(h) => lst
      case x::y::_ => {
        val encodedTail = encodeModified(lst.tail)
        if (x == y) {
          encodedTail.head match {
            case (num: Int, elem)  => (num + 1, elem) :: encodedTail.tail
            case _ => (2, x) :: encodedTail.tail
          }
        } else {
          List(x) :: encodedTail
        }
      }
    }
  }
}
