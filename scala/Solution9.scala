object Solution9 {
  def pack[T](lst: List[T]): List[List[T]] = {
    lst match {
      case List() => List(List())
      case List(h) => List(List(h))
      case x::y::_ => {
        val packtail = pack(lst.tail)
        if (x == y) (x :: packtail.head) :: packtail.tail
        else List(x) :: packtail
      }
    }
  }
}
