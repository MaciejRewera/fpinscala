package fpinscala.learning.parallelism

import fpinscala.exercises.parallelism.Nonblocking
import fpinscala.exercises.parallelism.Nonblocking.{Par as NPar, *}

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.Random

object ParallelismMain:

  private val executorService = Executors.newFixedThreadPool(1)

  @main def parallelismTest = {

    val a = NPar.lazyUnit(42 + 1)
    println(NPar.fork(NPar.fork(a)).run(executorService))

    executorService.shutdown()
  }

  private def measureExecutionTime[A](f: => A): (A, Duration) =
    val startTime = System.nanoTime()
    val result = f
    val endTime = System.nanoTime()
    val executionTime = Duration.apply((endTime - startTime), TimeUnit.NANOSECONDS)
    (result, executionTime)