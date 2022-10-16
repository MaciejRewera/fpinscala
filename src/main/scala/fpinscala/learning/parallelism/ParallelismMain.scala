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

    val (maxResult, maxTime) = measureExecutionTime { hugeList.max }
    println(s"maxResult: ${maxResult}")
    println(s"maxTime  : ${maxTime.toMillis} ms")

    val (maxParResult, maxParTime) = measureExecutionTime { Par.max(hugeList.toIndexedSeq) }
    println(s"maxParResult: ${maxParResult}")
    println(s"maxParTime: ${maxParTime.toMillis} ms")

    val (maxParRunnedResult, maxParRunnedTime) = measureExecutionTime { Par.run(executorService)(maxParResult).get() }
    println(s"maxParRunnedResult: ${maxParRunnedResult}")
    println(s"maxParRunnedTime  : ${maxParRunnedTime.toMillis} ms")


    val shortList = List(1, 2, 3, 4, 5, 6)

    val (sumParResult, sumParTime) = measureExecutionTime { Par.sum(shortList.toIndexedSeq) }
    println(s"sumParResult: ${sumParResult}")
    println(s"sumParTime  : ${sumParTime.toMillis} ms")

    val (sumParRunnedResult, sumParRunnedTime) = measureExecutionTime { Par.run(executorService)(sumParResult).get() }
    println(s"sumParRunnedResult: ${sumParRunnedResult}")
    println(s"sumParRunnedTime  : ${sumParRunnedTime.toMillis} ms")

    executorService.shutdown()
  }

  private def measureExecutionTime[A](f: => A): (A, Duration) =
    val startTime = System.nanoTime()
    val result = f
    val endTime = System.nanoTime()
    val executionTime = Duration.apply((endTime - startTime), TimeUnit.NANOSECONDS)
    (result, executionTime)