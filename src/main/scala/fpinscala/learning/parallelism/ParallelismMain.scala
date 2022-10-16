package fpinscala.learning.parallelism

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.Random

object ParallelismMain:

  private val executorService = Executors.newFixedThreadPool(10)

  @main def parallelismTest = {
    val filterFunc = (_: Int) % 2 == 0

    val (hugeList, hugeListCreationTime) = measureExecutionTime { List.fill(1000000)(Random.nextInt(1000)) }
    println(s"hugeListCreationTime: ${hugeListCreationTime.toMillis} ms")

    val (hugeListFilteredPar, hugeListFilteredParTime) = measureExecutionTime { Par.parFilter(hugeList)(filterFunc) }
    println(s"hugeListFilteredParTime: ${hugeListFilteredParTime.toMicros} micros")

    val (hugeListFilteredRunned, hugeListFilteredRunnedTime) = measureExecutionTime { Par.run(executorService)(hugeListFilteredPar).get() }
    println(s"Actual filtered list: ${hugeListFilteredRunned.take(100)}")
    println(s"Actual filtered list exec. time: ${hugeListFilteredRunnedTime.toMillis} ms")

    val (hugeListFiltered, hugeListFilteredTime) = measureExecutionTime { hugeList.filter(filterFunc) }
    println(s"Actual sequentially-filtered list: ${hugeListFiltered.take(100)}")
    println(s"Actual sequentially-filtered list exec. time: ${hugeListFilteredTime.toMillis} ms")

    executorService.shutdown()
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