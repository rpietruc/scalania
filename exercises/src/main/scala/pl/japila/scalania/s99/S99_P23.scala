package pl.japila.scalania.s99

object S99_P23 {
  type RandomSelectFn[T] = (Int, Seq[T]) => Seq[T]

  def solutions[T]: List[(String, RandomSelectFn[T])] = List(
    ("my own implementation", randomSelect)
  )

  def removeAt[T](n: Int, ts: Seq[T]): (Seq[T], T) =
    (ts.take(n) ++ ts.drop(n + 1), ts(n))

  def randomSelect[T](count: Int, ts: Seq[T]): Seq[T] = ts match {
    case Nil => Nil
    case _ => {
      if (count <= 0) Nil
      else {
        val res = removeAt(scala.util.Random.nextInt(ts.length), ts)
        res._2 +: randomSelect(count - 1, res._1)
      }
    }
  }
}
