package pl.japila.scalania.s99

object S99_P17 {
  def split[T](n: Int, ts: Seq[T]): (Seq[T], Seq[T]) =
    (
      //  ts.reverse.drop(ts.length - n).reverse,
      ts.zipWithIndex.filter(p => p._2 < n).map(_._1),
      ts.drop(n)
    )
}
