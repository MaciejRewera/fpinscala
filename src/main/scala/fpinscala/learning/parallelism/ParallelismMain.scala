package fpinscala.learning.parallelism

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.Duration

object ParallelismMain:

  @main def parallelismTest = {
    val ints = IndexedSeq(1, 2, 3, 4, 5, 6)

    println(sum(ints))

    val (hugeList, hugeListCreationTime) = measureExecutionTime { List.fill(1000000)(7) }
    val (hugeListMapped, hugeListMappedCreationTime) = measureExecutionTime { Par.parMap(hugeList)(_ + 1) }
    val (hugeListMappedSlow, hugeListMappedSlowCreationTime) = measureExecutionTime { Par.parMapSlow(hugeList)(_ + 1) }

    println(s"hugeListMapped: ${hugeListMapped}")
    println(s"hugeListCreationTime          : ${hugeListCreationTime.toMillis} ms")
    println(s"hugeListMappedCreationTime    : ${hugeListMappedCreationTime.toMicros} micros")
    println(s"hugeListMappedSlowCreationTime: ${hugeListMappedSlowCreationTime.toMillis} ms")

  }

  private def sum(ints: IndexedSeq[Int]): Par[Int] =
    if ints.size <= 1 then Par.unit(ints.headOption.getOrElse(0))
    else
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(sum(l), sum(r))(_ + _)

  private def measureExecutionTime[A](f: => A): (A, Duration) =
    val startTime = System.nanoTime()
    val result = f
    val endTime = System.nanoTime()
    val executionTime = Duration.apply((endTime - startTime), TimeUnit.NANOSECONDS)
    (result, executionTime)