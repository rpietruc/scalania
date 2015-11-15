package pl.japila.scalania.s99

object S99_P18 {
  def slice[T](from: Int, to: Int, ts: Seq[T]): Seq[T] =
    ts.zipWithIndex.filter(e => (e._2 >= from) && (e._2 < to)).map(_._1)
}
