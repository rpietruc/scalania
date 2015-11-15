package pl.japila.scalania.s99

object S99_P22 {

  type RangeFn = (Int, Int) => Seq[Int]

  val solutions: List[(String, RangeFn)] = List(
    ("my own implementation", range)
  )

  def range(from: Int, to: Int): Seq[Int] =
    rangeFn(Seq(from), to)
  def rangeFn(ts: Seq[Int], to: Int): Seq[Int] =
    if (ts.reverse.head >= to) ts
    else rangeFn(ts :+ (ts.reverse.head + 1), to)
}
