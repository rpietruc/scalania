package pl.japila.scalania.s99

object S99_P15 {
  def duplicateN[T](n: Int, ts: Seq[T]): Seq[T] =
    ts.flatMap(e => List.fill(n)(e))
}
