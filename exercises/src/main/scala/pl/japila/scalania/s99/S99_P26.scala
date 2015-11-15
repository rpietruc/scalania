package pl.japila.scalania.s99

object S99_P26 {
  type CombinationsFn[T] = (Int, Seq[T]) => Seq[Seq[T]]

  def solutions[T](): List[(String, CombinationsFn[T])] = List(
    ("my own implementation", combinations[T])
  )

  def combinations[T](count: Int, ts: Seq[T]): Seq[Seq[T]] = {
    def foldFun[T](sc: (Seq[Seq[T]], Int), e: T): (Seq[Seq[T]], Int) = {
      (
        if (sc._2 <= 0)
          sc._1.flatMap(v => if (v.size < count) Seq(v :+ e, v) else Seq(v)) :+ Seq(e)
        else
          sc._1.flatMap(v => if (v.size < count) { if (v.size > sc._2) Seq(v :+ e, v) else Seq(v :+ e) } else Seq(v)), sc._2 + 1)
    }
    ts.foldLeft((Seq[Seq[T]]()), count - ts.size)(foldFun)._1
  }
}
