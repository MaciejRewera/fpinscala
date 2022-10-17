package fpinscala.learning.parallelism

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.Random

object ParallelismMain:

  private val executorService = Executors.newFixedThreadPool(10)

  @main def parallelismTest = {
    def filterFunc(i: Int): Boolean = {
      Thread.sleep(1)
      i % 2 == 0
    }

    val (hugeList, hugeListCreationTime) = measureExecutionTime { List.fill(1000)(Random.nextInt(1000)) }
    println(s"hugeListCreationTime: ${hugeListCreationTime.toMillis} ms")

    val (hugeListMapped, hugeListMappedCreationTime) = measureExecutionTime { Par.parMap(hugeList)(_ + 1) }
    println(s"hugeListMapped: ${hugeListMapped}")
    println(s"hugeListMappedCreationTime    : ${hugeListMappedCreationTime.toMicros} micros")

    val (hugeListMappedSlow, hugeListMappedSlowCreationTime) = measureExecutionTime { Par.parMapSlow(hugeList)(_ + 1) }
    println(s"hugeListMappedSlowCreationTime: ${hugeListMappedSlowCreationTime.toMillis} ms")


    val (hugeListFilteredPar, hugeListFilteredParTime) = measureExecutionTime { Par.parFilter(hugeList)(filterFunc) }
    println(s"hugeListFilteredParTime: ${hugeListFilteredParTime.toMicros} micros")

    val (hugeListFiltered, hugeListFilteredTime) = measureExecutionTime { hugeList.filter(filterFunc) }
    println(s"Actual sequentially-filtered list: ${hugeListFiltered.take(100)}")
    println(s"Actual sequentially-filtered list exec. time: ${hugeListFilteredTime.toMillis} ms")

    val (hugeListFilteredRunned, hugeListFilteredRunnedTime) = measureExecutionTime { Par.run(executorService)(hugeListFilteredPar).get() }
    println(s"Actual filtered list: ${hugeListFilteredRunned.take(100)}")
    println(s"Actual filtered list exec. time: ${hugeListFilteredRunnedTime.toMillis} ms")

    executorService.shutdown()
  }

  private def measureExecutionTime[A](f: => A): (A, Duration) =
    val startTime = System.nanoTime()
    val result = f
    val endTime = System.nanoTime()
    val executionTime = Duration.apply((endTime - startTime), TimeUnit.NANOSECONDS)
    (result, executionTime)