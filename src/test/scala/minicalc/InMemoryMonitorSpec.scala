package minicalc.monitoring

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import cats.syntax.parallel._
import cats.syntax.traverse._
import minicalc.task._
import scala.concurrent.duration._

class InMemoryMonitorSpec extends AnyFlatSpec with Matchers {

  "InMemoryMonitor.create" should "initialize with empty state" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      stats <- monitor.getStatistics
    } yield {
      stats.tasksStarted shouldBe 0
      stats.tasksCompleted shouldBe 0
      stats.tasksFailed shouldBe 0
      stats.averageDuration shouldBe 0.seconds
      stats.currentlyRunning shouldBe empty
    }
    
    test.unsafeRunSync()
  }

  "onTaskStart" should "increment tasksStarted and add to currentlyRunning" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      taskId1 = TaskId("task-1")
      taskId2 = TaskId("task-2")
      _ <- monitor.onTaskStart(taskId1)
      _ <- monitor.onTaskStart(taskId2)
      stats <- monitor.getStatistics
    } yield {
      stats.tasksStarted shouldBe 2
      stats.currentlyRunning should contain allOf (taskId1, taskId2)
      stats.tasksCompleted shouldBe 0
      stats.tasksFailed shouldBe 0
    }
    
    test.unsafeRunSync()
  }

  "onTaskComplete with Success" should "increment tasksCompleted and remove from currentlyRunning" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      taskId = TaskId("task-1")
      _ <- monitor.onTaskStart(taskId)
      result = Success(taskId, "result", 100.millis)
      _ <- monitor.onTaskComplete(result)
      stats <- monitor.getStatistics
    } yield {
      stats.tasksStarted shouldBe 1
      stats.tasksCompleted shouldBe 1
      stats.tasksFailed shouldBe 0
      stats.currentlyRunning shouldBe empty
      stats.averageDuration shouldBe 100.millis
    }
    
    test.unsafeRunSync()
  }

  "onTaskComplete with Failure" should "increment tasksFailed and remove from currentlyRunning" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      taskId = TaskId("task-1")
      _ <- monitor.onTaskStart(taskId)
      error = ExecutionError("Something went wrong", None)
      result = Failure(taskId, error, 150.millis)
      _ <- monitor.onTaskComplete(result)
      stats <- monitor.getStatistics
    } yield {
      stats.tasksStarted shouldBe 1
      stats.tasksCompleted shouldBe 0
      stats.tasksFailed shouldBe 1
      stats.currentlyRunning shouldBe empty
      stats.averageDuration shouldBe 150.millis
    }
    
    test.unsafeRunSync()
  }

  "onTaskComplete with Skipped" should "only remove from currentlyRunning" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      taskId = TaskId("task-1")
      _ <- monitor.onTaskStart(taskId)
      result = Skipped(taskId, "Dependencies failed")
      _ <- monitor.onTaskComplete(result)
      stats <- monitor.getStatistics
    } yield {
      stats.tasksStarted shouldBe 1
      stats.tasksCompleted shouldBe 0
      stats.tasksFailed shouldBe 0
      stats.currentlyRunning shouldBe empty
      stats.averageDuration shouldBe 0.seconds // No duration for skipped tasks
    }
    
    test.unsafeRunSync()
  }

  "averageDuration" should "be computed correctly from multiple tasks" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      task1 = TaskId("task-1")
      task2 = TaskId("task-2")
      task3 = TaskId("task-3")
      _ <- monitor.onTaskStart(task1)
      _ <- monitor.onTaskStart(task2)
      _ <- monitor.onTaskStart(task3)
      _ <- monitor.onTaskComplete(Success(task1, "result", 100.millis))
      _ <- monitor.onTaskComplete(Success(task2, "result", 200.millis))
      _ <- monitor.onTaskComplete(Failure(task3, ExecutionError("error", None), 300.millis))
      stats <- monitor.getStatistics
    } yield {
      stats.tasksStarted shouldBe 3
      stats.tasksCompleted shouldBe 2
      stats.tasksFailed shouldBe 1
      stats.currentlyRunning shouldBe empty
      // Average: (100 + 200 + 300) / 3 = 200ms
      stats.averageDuration shouldBe 200.millis
    }
    
    test.unsafeRunSync()
  }

  "averageDuration" should "handle division by zero when no tasks finished" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      taskId = TaskId("task-1")
      _ <- monitor.onTaskStart(taskId)
      stats <- monitor.getStatistics
    } yield {
      stats.tasksStarted shouldBe 1
      stats.averageDuration shouldBe 0.seconds
    }
    
    test.unsafeRunSync()
  }

  "averageDuration" should "include failed tasks but not skipped tasks" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      task1 = TaskId("task-1")
      task2 = TaskId("task-2")
      task3 = TaskId("task-3")
      _ <- monitor.onTaskStart(task1)
      _ <- monitor.onTaskStart(task2)
      _ <- monitor.onTaskStart(task3)
      _ <- monitor.onTaskComplete(Success(task1, "result", 100.millis))
      _ <- monitor.onTaskComplete(Failure(task2, ExecutionError("error", None), 200.millis))
      _ <- monitor.onTaskComplete(Skipped(task3, "Skipped"))
      stats <- monitor.getStatistics
    } yield {
      stats.tasksCompleted shouldBe 1
      stats.tasksFailed shouldBe 1
      // Average: (100 + 200) / 2 = 150ms (skipped task3 doesn't count)
      stats.averageDuration shouldBe 150.millis
    }
    
    test.unsafeRunSync()
  }

  "reset" should "clear all state" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      task1 = TaskId("task-1")
      task2 = TaskId("task-2")
      _ <- monitor.onTaskStart(task1)
      _ <- monitor.onTaskStart(task2)
      _ <- monitor.onTaskComplete(Success(task1, "result", 100.millis))
      statsBeforeReset <- monitor.getStatistics
      _ <- monitor.reset
      statsAfterReset <- monitor.getStatistics
    } yield {
      // Before reset
      statsBeforeReset.tasksStarted shouldBe 2
      statsBeforeReset.tasksCompleted shouldBe 1
      statsBeforeReset.currentlyRunning should contain(task2)
      
      // After reset
      statsAfterReset.tasksStarted shouldBe 0
      statsAfterReset.tasksCompleted shouldBe 0
      statsAfterReset.tasksFailed shouldBe 0
      statsAfterReset.averageDuration shouldBe 0.seconds
      statsAfterReset.currentlyRunning shouldBe empty
    }
    
    test.unsafeRunSync()
  }

  "monitor" should "handle concurrent updates correctly" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      tasks = (1 to 10).map(i => TaskId(s"task-$i")).toList
      // Start all tasks concurrently
      _ <- tasks.parTraverse(taskId => monitor.onTaskStart(taskId))
      // Complete half successfully, half failed
      _ <- tasks.take(5).parTraverse(taskId => 
        monitor.onTaskComplete(Success(taskId, "result", 50.millis))
      )
      _ <- tasks.drop(5).parTraverse(taskId => 
        monitor.onTaskComplete(Failure(taskId, ExecutionError("error", None), 100.millis))
      )
      stats <- monitor.getStatistics
    } yield {
      stats.tasksStarted shouldBe 10
      stats.tasksCompleted shouldBe 5
      stats.tasksFailed shouldBe 5
      stats.currentlyRunning shouldBe empty
      // Average: (5 * 50 + 5 * 100) / 10 = 75ms
      stats.averageDuration shouldBe 75.millis
    }
    
    test.unsafeRunSync()
  }

  "currentlyRunning" should "track only tasks that haven't completed" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      task1 = TaskId("task-1")
      task2 = TaskId("task-2")
      task3 = TaskId("task-3")
      _ <- monitor.onTaskStart(task1)
      _ <- monitor.onTaskStart(task2)
      _ <- monitor.onTaskStart(task3)
      stats1 <- monitor.getStatistics
      _ <- monitor.onTaskComplete(Success(task1, "result", 100.millis))
      stats2 <- monitor.getStatistics
      _ <- monitor.onTaskComplete(Failure(task2, ExecutionError("error", None), 100.millis))
      stats3 <- monitor.getStatistics
    } yield {
      stats1.currentlyRunning should contain allOf (task1, task2, task3)
      stats2.currentlyRunning should contain allOf (task2, task3)
      stats2.currentlyRunning should not contain task1
      stats3.currentlyRunning should contain only task3
    }
    
    test.unsafeRunSync()
  }

  "monitor" should "handle tasks with varying durations" in {
    val test = for {
      monitor <- InMemoryMonitor.create
      task1 = TaskId("task-1")
      task2 = TaskId("task-2")
      task3 = TaskId("task-3")
      _ <- monitor.onTaskStart(task1)
      _ <- monitor.onTaskStart(task2)
      _ <- monitor.onTaskStart(task3)
      _ <- monitor.onTaskComplete(Success(task1, "result", 1.second))
      _ <- monitor.onTaskComplete(Success(task2, "result", 500.millis))
      _ <- monitor.onTaskComplete(Success(task3, "result", 2.seconds))
      stats <- monitor.getStatistics
    } yield {
      stats.tasksCompleted shouldBe 3
      // Average: (1000 + 500 + 2000) / 3 = 1166.67ms ≈ 1166ms
      stats.averageDuration.toMillis shouldBe 1166L +- 1L
    }
    
    test.unsafeRunSync()
  }
}