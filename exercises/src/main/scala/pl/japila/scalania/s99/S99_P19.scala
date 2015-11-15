package pl.japila.scalania.s99

object S99_P19 {
  def rotate[T](n: Int, ts: Seq[T]): Seq[T] = ts match {
    case Nil => Nil
    case _ => {

      def positiveModulo(n: Int, m: Int): Int =
        if (n % m < 0)
          m + n % m
        else
          n % m

      val N = positiveModulo(n, ts.length)
      val l = ts.zipWithIndex.filter(e => e._2 >= N).map(_._1)
      val r = ts.zipWithIndex.filter(e => e._2 < N).map(_._1)
      l ++ r
    }
  }
}

