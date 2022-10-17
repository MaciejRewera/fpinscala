package fpinscala.learning.parallelism

import java.util.concurrent.{ExecutorService, Executors, TimeUnit}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.Random

object ParallelismMain:

  private val executorService = Executors.newFixedThreadPool(10)

  @main def parallelismTest = {

    val singleParagraph = "This repository contains exercises, hints, and answers for the book Functional Programming in Scala. Along with the book itself, it's the closest you'll get to having your own private functional programming tutor without actually having one."
    val singleParagraphLength = 36
    val (paragraphs, paragraphsCreationTime) = measureExecutionTime { List.fill(1000000)(singleParagraph) }
    println(s"paragraphsCreationTime: ${paragraphsCreationTime.toMillis} ms")

    val (paragraphsCountedSequentially, paragraphsCountedSequentiallyTime) =
      measureExecutionTime { paragraphs.foldLeft(0)((sum, p) => p.split(' ').length + sum) }
    println(s"paragraphsCountedSequentially    : ${paragraphsCountedSequentially}")
    println(s"paragraphsCountedSequentiallyTime: ${paragraphsCountedSequentiallyTime.toMillis} ms")

    val (paragraphsCounted, paragraphsCountedTime) = measureExecutionTime { Par.countParagraphs(paragraphs) }
    println(s"paragraphsCounted    : ${paragraphsCounted}")
    println(s"paragraphsCountedTime: ${paragraphsCountedTime.toMillis} ms")

    val (paragraphsCountedRunned, paragraphsCountedRunnedTime) = measureExecutionTime { paragraphsCounted(executorService).get() }
    println(s"paragraphsCountedRunned    : ${paragraphsCountedRunned}")
    println(s"paragraphsCountedRunnedTime: ${paragraphsCountedRunnedTime.toMillis} ms")

    executorService.shutdown()
  }

  private def measureExecutionTime[A](f: => A): (A, Duration) =
    val startTime = System.nanoTime()
    val result = f
    val endTime = System.nanoTime()
    val executionTime = Duration.apply((endTime - startTime), TimeUnit.NANOSECONDS)
    (result, executionTime)