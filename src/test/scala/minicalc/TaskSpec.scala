package minicalc.task

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._

/**
 * Tests for the task package types.
 */
class TaskSpec extends AnyFlatSpec with Matchers {

  // ============================================
  // TASK ID TESTS
  // ============================================

  "TaskId" should "generate unique IDs" in {
    val id1 = TaskId.generate()
    val id2 = TaskId.generate()
    id1 should not equal id2
  }

  it should "generate IDs with prefix" in {
    val id = TaskId.generate("myPrefix")
    id.value should startWith("myprefix-")
  }

  it should "validate correct IDs" in {
    TaskId.isValid("task-123") shouldBe true
    TaskId.isValid("my_task") shouldBe true
    TaskId.isValid("task.subtask") shouldBe true
    TaskId.isValid("Task-Name_123") shouldBe true
  }

  it should "reject invalid IDs" in {
    TaskId.isValid("") shouldBe false
    TaskId.isValid("task with spaces") shouldBe false
    TaskId.isValid("task@name") shouldBe false
  }

  it should "sanitize strings into valid IDs" in {
    TaskId.sanitize("My Task Name") shouldBe "my-task-name"
    TaskId.sanitize("task@#$%name") shouldBe "taskname"
    TaskId.sanitize("  spaces  ") shouldBe "spaces"
    TaskId.sanitize("") shouldBe "task"
  }

  it should "create child IDs" in {
    val parent = TaskId("parent")
    val child = parent.child("child")
    child.value shouldBe "parent.child"
  }

  it should "create IDs with suffix" in {
    val base = TaskId("task")
    val withSuffix = base.withSuffix("retry")
    withSuffix.value shouldBe "task-retry"
  }

  // ============================================
  // PRIORITY TESTS
  // ============================================

  "Priority" should "order High > Normal > Low" in {
    High.compare(Normal) should be > 0
    Normal.compare(Low) should be > 0
    High.compare(Low) should be > 0
  }

  it should "consider equal priorities equal" in {
    High.compare(High) shouldBe 0
    Normal.compare(Normal) shouldBe 0
    Low.compare(Low) shouldBe 0
  }

  it should "parse from string" in {
    Priority.fromString("High") shouldBe Some(High)
    Priority.fromString("Normal") shouldBe Some(Normal)
    Priority.fromString("Low") shouldBe Some(Low)
    Priority.fromString("invalid") shouldBe None
    Priority.fromString("high") shouldBe None // Case sensitive
  }

  it should "have implicit ordering" in {
    val priorities = List(Low, High, Normal, Low, High)
    priorities.sorted shouldBe List(Low, Low, Normal, High, High)
  }

  // ============================================
  // RETRY POLICY TESTS
  // ============================================

  "NoRetry" should "never allow retries" in {
    NoRetry.shouldRetry(1) shouldBe false
    NoRetry.shouldRetry(0) shouldBe false
    NoRetry.shouldRetry(100) shouldBe false
  }

  it should "have zero delay" in {
    NoRetry.delayForAttempt(1) shouldBe 0.seconds
  }

  "LinearRetry" should "allow retries up to maxAttempts" in {
    val policy = LinearRetry(3, 1.second)
    policy.shouldRetry(1) shouldBe true
    policy.shouldRetry(2) shouldBe true
    policy.shouldRetry(3) shouldBe false
    policy.shouldRetry(4) shouldBe false
  }

  it should "have constant delay" in {
    val policy = LinearRetry(3, 5.seconds)
    policy.delayForAttempt(1) shouldBe 5.seconds
    policy.delayForAttempt(2) shouldBe 5.seconds
    policy.delayForAttempt(3) shouldBe 5.seconds
  }

  it should "require positive maxAttempts" in {
    an[IllegalArgumentException] should be thrownBy LinearRetry(0, 1.second)
    an[IllegalArgumentException] should be thrownBy LinearRetry(-1, 1.second)
  }

  it should "require maxAttempts <= 100" in {
    an[IllegalArgumentException] should be thrownBy LinearRetry(101, 1.second)
  }

  "ExponentialRetry" should "allow retries up to maxAttempts" in {
    val policy = ExponentialRetry(3, 1.second)
    policy.shouldRetry(1) shouldBe true
    policy.shouldRetry(2) shouldBe true
    policy.shouldRetry(3) shouldBe false
  }

  it should "have exponentially increasing delay" in {
    val policy = ExponentialRetry(5, 1.second)
    policy.delayForAttempt(1) shouldBe 1.second
    policy.delayForAttempt(2) shouldBe 2.seconds
    policy.delayForAttempt(3) shouldBe 4.seconds
    policy.delayForAttempt(4) shouldBe 8.seconds
  }

  it should "require maxAttempts <= 25" in {
    an[IllegalArgumentException] should be thrownBy ExponentialRetry(26, 1.second)
  }

  // ============================================
  // TASK BUILDER TESTS
  // ============================================

  "TaskBuilder" should "create tasks with default values" in {
    val task = TaskBuilder.task("myTask")(42).build

    task.name shouldBe "myTask"
    task.priority shouldBe Normal
    task.dependencies shouldBe empty
    task.timeout shouldBe None
    task.retryPolicy shouldBe NoRetry
    task.metadata shouldBe empty
  }

  it should "set priority" in {
    val task = TaskBuilder.task("myTask")(42)
      .withPriority(High)
      .build

    task.priority shouldBe High
  }

  it should "set dependencies" in {
    val dep1 = TaskId("dep1")
    val dep2 = TaskId("dep2")
    val task = TaskBuilder.task("myTask")(42)
      .dependsOn(dep1, dep2)
      .build

    task.dependencies should contain(dep1)
    task.dependencies should contain(dep2)
  }

  it should "set timeout" in {
    val task = TaskBuilder.task("myTask")(42)
      .withTimeout(30.seconds)
      .build

    task.timeout shouldBe Some(30.seconds)
  }

  it should "set linear retry" in {
    val task = TaskBuilder.task("myTask")(42)
      .withRetry(3, 5.seconds)
      .build

    task.retryPolicy shouldBe a[LinearRetry]
    task.retryPolicy.asInstanceOf[LinearRetry].maxAttempts shouldBe 3
    task.retryPolicy.asInstanceOf[LinearRetry].delay shouldBe 5.seconds
  }

  it should "set exponential retry" in {
    val task = TaskBuilder.task("myTask")(42)
      .withExponentialRetry(5, 1.second)
      .build

    task.retryPolicy shouldBe a[ExponentialRetry]
    task.retryPolicy.asInstanceOf[ExponentialRetry].maxAttempts shouldBe 5
  }

  it should "add metadata" in {
    val task = TaskBuilder.task("myTask")(42)
      .addMetadata("key1", "value1")
      .addMetadata("key2", "value2")
      .build

    task.metadata should contain("key1" -> "value1")
    task.metadata should contain("key2" -> "value2")
  }

  it should "support chaining all options" in {
    val task = TaskBuilder.task("myTask")(42)
      .withId(TaskId("custom-id"))
      .withPriority(High)
      .dependsOn(TaskId("dep1"))
      .withTimeout(30.seconds)
      .withRetry(3, 5.seconds)
      .addMetadata("env", "test")
      .build

    task.id shouldBe TaskId("custom-id")
    task.priority shouldBe High
    task.dependencies should contain(TaskId("dep1"))
    task.timeout shouldBe Some(30.seconds)
    task.retryPolicy shouldBe a[LinearRetry]
    task.metadata should contain("env" -> "test")
  }

  it should "create tasks with IO actions" in {
    val task = TaskBuilder.taskIO("ioTask")(IO.pure(42)).build

    task.name shouldBe "ioTask"
    task.action.unsafeRunSync() shouldBe 42
  }

  // ============================================
  // TASK RESULT TESTS
  // ============================================

  "Success" should "contain the value" in {
    val result: TaskResult[Int] = Success(TaskId("t1"), 42, 1.second)
    result.isSuccess shouldBe true
    result.isFailure shouldBe false
    result.isSkipped shouldBe false
    result.toOption shouldBe Some(42)
  }

  "Failure" should "contain the error" in {
    val error = ExecutionError("Something went wrong", None)
    val result: TaskResult[Int] = Failure(TaskId("t1"), error, 1.second)
    result.isSuccess shouldBe false
    result.isFailure shouldBe true
    result.isSkipped shouldBe false
    result.toOption shouldBe None
  }

  "Skipped" should "contain the reason" in {
    val result: TaskResult[Int] = Skipped(TaskId("t1"), "Dependency failed")
    result.isSuccess shouldBe false
    result.isFailure shouldBe false
    result.isSkipped shouldBe true
    result.toOption shouldBe None
  }

  // ============================================
  // TASK ERROR TESTS
  // ============================================

  "ExecutionError" should "include message and cause" in {
    val cause = new RuntimeException("Boom")
    val error = ExecutionError("Task failed", Some(cause))
    error.message shouldBe "Task failed"
  }

  "TimeoutError" should "include duration" in {
    val error = TimeoutError(30.seconds)
    error.message should include("30 seconds")
    error.message should include("timed out")
  }

  "DependencyError" should "list missing dependencies" in {
    val error = DependencyError(Set(TaskId("dep1"), TaskId("dep2")))
    error.message should include("dep1")
    error.message should include("dep2")
  }

  "CircularDependencyError" should "show cycle" in {
    val error = CircularDependencyError(List(TaskId("a"), TaskId("b"), TaskId("a")))
    error.message should include("Circular")
    error.message should include("a")
    error.message should include("b")
  }
}