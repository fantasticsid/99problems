object Solution7 {
  def flatten(lst: List[Any]): List[Any] = {
    if (lst.isEmpty) lst
    else {
      val h = lst.head
      h match  {
        case l: List[Any] => flatten(l) ++ flatten(lst.tail)
        case _ => h::flatten(lst.tail)
      }
    }
  }
}
