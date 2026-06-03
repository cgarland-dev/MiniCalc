package minicalc.execution

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

import minicalc.scheduler.TopologicalScheduler
import minicalc.task._

/**
 * Tests for the FiberExecutionEngine.
 */
class FiberExecutionEngineSpec extends AnyFlatSpec with Matchers {

  val scheduler = new TopologicalScheduler()
  val engine = new FiberExecutionEngine(scheduler)
  val config = ExecutionConfig.default

  // Helper to create simple tasks
  def makeTask[A](name: String, action: IO[A]): Task[A] = {
    TaskBuilder.taskIO(name)(action)
      .withId(TaskId(name))
      .build
  }

  def makeTaskWithTimeout[A](name: String, action: IO[A], timeout: FiniteDuration): Task[A] = {
    TaskBuilder.taskIO(name)(action)
      .withId(TaskId(name))
      .withTimeout(timeout)
      .build
  }

  def makeTaskWithRetry[A](name: String, action: IO[A], attempts: Int, delay: FiniteDuration): Task[A] = {
    TaskBuilder.taskIO(name)(action)
      .withId(TaskId(name))
      .withRetry(attempts, delay)
      .build
  }

  // ============================================
  // SINGLE TASK EXECUTION
  // ============================================

  "FiberExecutionEngine" should "execute a successful task" in {
    val task = makeTask("success", IO.pure(42))
    val report = engine.execute(List(task), config).unsafeRunSync()

    report.totalTasks shouldBe 1
    report.successful shouldBe 1
    report.failed shouldBe 0
    report.results should have size 1
    report.results.head.isSuccess shouldBe true
    report.results.head.toOption shouldBe Some(42)
  }

  it should "execute a failing task" in {
    val task = makeTask("fail", IO.raiseError(new RuntimeException("Boom")))
    val report = engine.execute(List(task), config).unsafeRunSync()

    report.totalTasks shouldBe 1
    report.successful shouldBe 0
    report.failed shouldBe 1
    report.results.head.isFailure shouldBe true
  }

  it should "capture error message in failure" in {
    val task = makeTask("fail", IO.raiseError(new RuntimeException("Custom error message")))
    val report = engine.execute(List(task), config).unsafeRunSync()

    val failure = report.results.head.asInstanceOf[Failure[Int]]
    failure.error.message should include("Custom error message")
  }

  // ============================================
  // CONCURRENT EXECUTION
  // ============================================

  it should "execute multiple tasks concurrently" in {
    val tasks = List(
      makeTask("t1", IO.pure(1)),
      makeTask("t2", IO.pure(2)),
      makeTask("t3", IO.pure(3))
    )
    val report = engine.execute(tasks, config).unsafeRunSync()

    report.totalTasks shouldBe 3
    report.successful shouldBe 3
    report.failed shouldBe 0
  }

  it should "respect maxConcurrency limit" in {
    var maxConcurrent = 0
    var currentConcurrent = 0
    val lock = new Object()

    def trackingTask(name: String): Task[Int] = {
      makeTask(name, IO {
        lock.synchronized {
          currentConcurrent += 1
          if (currentConcurrent > maxConcurrent) {
            maxConcurrent = currentConcurrent
          }
        }
        Thread.sleep(50)
        lock.synchronized {
          currentConcurrent -= 1
        }
        42
      })
    }

    val tasks = (1 to 10).map(i => trackingTask(s"t$i")).toList
    val limitedConfig = ExecutionConfig.default.copy(maxConcurrency = 3)
    val report = engine.execute(tasks, limitedConfig).unsafeRunSync()

    report.successful shouldBe 10
    maxConcurrent should be <= 3
  }

  it should "continue executing other tasks when one fails" in {
    val tasks = List(
      makeTask("t1", IO.pure(1)),
      makeTask("fail", IO.raiseError(new RuntimeException("Boom"))),
      makeTask("t3", IO.pure(3))
    )
    val report = engine.execute(tasks, config).unsafeRunSync()

    report.totalTasks shouldBe 3
    report.successful shouldBe 2
    report.failed shouldBe 1
  }

  // ============================================
  // TIMEOUT HANDLING
  // ============================================

  it should "timeout slow tasks" in {
    val task = makeTaskWithTimeout("slow", IO.sleep(5.seconds) >> IO.pure(42), 100.millis)
    val report = engine.execute(List(task), config).unsafeRunSync()

    report.failed shouldBe 1
    val failure = report.results.head.asInstanceOf[Failure[Int]]
    failure.error shouldBe a[TimeoutError]
  }

  it should "complete fast tasks before timeout" in {
    val task = makeTaskWithTimeout("fast", IO.pure(42), 5.seconds)
    val report = engine.execute(List(task), config).unsafeRunSync()

    report.successful shouldBe 1
    report.results.head.toOption shouldBe Some(42)
  }

  // ============================================
  // RETRY POLICIES
  // ============================================

  it should "retry failing tasks according to policy" in {
    var attempts = 0

    val task = makeTaskWithRetry("retry",
      IO {
        attempts += 1
        if (attempts < 3) throw new RuntimeException(s"Attempt $attempts failed")
        else 42
      },
      attempts = 3,
      delay = 10.millis
    )
    val report = engine.execute(List(task), config).unsafeRunSync()

    report.successful shouldBe 1
    attempts shouldBe 3
  }

  it should "fail after exhausting retries" in {
    var attempts = 0

    val task = makeTaskWithRetry("retry",
      IO {
        attempts += 1
        throw new RuntimeException(s"Attempt $attempts failed")
      },
      attempts = 3,
      delay = 10.millis
    )
    val report = engine.execute(List(task), config).unsafeRunSync()

    report.failed shouldBe 1
    attempts shouldBe 3
    val failure = report.results.head.asInstanceOf[Failure[Int]]
    failure.error.message should include("3 attempts")
  }

  it should "not retry NoRetry tasks" in {
    var attempts = 0

    val task = makeTask("noretry", IO {
      attempts += 1
      throw new RuntimeException("Fail")
    })
    val report = engine.execute(List(task), config).unsafeRunSync()

    report.failed shouldBe 1
    attempts shouldBe 1
  }

  // ============================================
  // EXECUTION REPORTS
  // ============================================

  it should "track total duration" in {
    val task = makeTask("timed", IO.sleep(100.millis) >> IO.pure(42))
    val report = engine.execute(List(task), config).unsafeRunSync()

    report.totalDuration should be >= 100.millis
  }

  it should "track individual task durations" in {
    val task = makeTask("timed", IO.sleep(100.millis) >> IO.pure(42))
    val report = engine.execute(List(task), config).unsafeRunSync()

    val success = report.results.head.asInstanceOf[Success[Int]]
    success.duration should be >= 100.millis
  }

  it should "calculate success rate" in {
    val tasks = List(
      makeTask("t1", IO.pure(1)),
      makeTask("t2", IO.pure(2)),
      makeTask("fail", IO.raiseError(new RuntimeException("Boom"))),
      makeTask("t4", IO.pure(4))
    )
    val report = engine.execute(tasks, config).unsafeRunSync()

    report.successRate shouldBe 0.75 +- 0.01
  }

  // ============================================
  // EXECUTE WITH DEPENDENCIES
  // ============================================

  it should "execute tasks respecting dependencies" in {
    val dep = makeTask("dep", IO.pure(1))
    val dependent = TaskBuilder.taskIO("dependent")(IO.pure(2))
      .withId(TaskId("dependent"))
      .dependsOn(TaskId("dep"))
      .build

    val report = engine.executeWithDependencies(List(dependent, dep)).unsafeRunSync()
    report.successful shouldBe 2
  }

  it should "fail all tasks when circular dependency detected" in {
    val a = TaskBuilder.taskIO("a")(IO.pure(1))
      .withId(TaskId("a"))
      .dependsOn(TaskId("b"))
      .build

    val b = TaskBuilder.taskIO("b")(IO.pure(2))
      .withId(TaskId("b"))
      .dependsOn(TaskId("a"))
      .build

    val report = engine.executeWithDependencies(List(a, b)).unsafeRunSync()
    report.failed shouldBe 2
    report.results.head.asInstanceOf[Failure[Int]].error.isInstanceOf[minicalc.task.CircularDependencyError] shouldBe true
  }

  // ============================================
  // EMPTY TASK LIST
  // ============================================

  it should "handle empty task list" in {
    val report = engine.execute(List.empty[Task[Int]], config).unsafeRunSync()

    report.totalTasks shouldBe 0
    report.successful shouldBe 0
    report.failed shouldBe 0
    report.results shouldBe empty
  }
}