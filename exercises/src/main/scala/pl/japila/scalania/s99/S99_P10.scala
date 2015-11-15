package pl.japila.scalania.s99

object S99_P10 {
  def encode[T](ts: Seq[T]): Seq[(Int, T)] = ts match {
    case Nil => Nil
    case _ => ts.foldLeft((Seq((0, ts.head))))(caseFn).reverse
  }
  def caseFn[T](res: (Seq[(Int, T)]), t: T): Seq[(Int, T)] =
    if (t == res.head._2)
      (res.head._1 + 1, res.head._2) +: res.tail
    else
      (1, t) +: res
}
