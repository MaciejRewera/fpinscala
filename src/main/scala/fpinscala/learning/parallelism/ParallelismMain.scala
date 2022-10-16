package fpinscala.learning.parallelism

object ParallelismMain:

  @main def parallelismTest = {
    val ints = IndexedSeq(1, 2, 3, 4, 5, 6)

    println(sum(ints))
  }

  private def sum(ints: IndexedSeq[Int]): Par[Int] =
    if ints.size <= 1 then Par.unit(ints.headOption.getOrElse(0))
    else
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)
