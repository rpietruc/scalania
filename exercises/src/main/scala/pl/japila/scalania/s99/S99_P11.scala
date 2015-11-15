package pl.japila.scalania.s99

object S99_P11 {
  def encodeModified[T](ts: Seq[T]): Seq[Either[(Int, T), T]] = ts match {
    case Nil => Nil
    case _ => ts.foldLeft((Seq((0, ts.head))))(caseFn).reverse.map(mapFn)
  }
  def mapFn[T](e: (Int, T)): Either[(Int, T), T] =
    if (e._1 == 1)
      Right(e._2)
    else
      Left(e)
  def caseFn[T](res: (Seq[(Int, T)]), t: T): Seq[(Int, T)] =
    if (t == res.head._2)
      (res.head._1 + 1, res.head._2) +: res.tail
    else
      (1, t) +: res
}
