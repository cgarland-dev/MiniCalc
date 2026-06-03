package minicalc.ast

import cats.effect.IO
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import minicalc.ast._
import minicalc.task._

import scala.concurrent.duration._

/**
 * Tests for Value types and their methods.
 */
class ValueSpec extends AnyFlatSpec with Matchers {

  // ============================================
  // NUM VALUE
  // ============================================

  "NumValue" should "extract as number" in {
    val v = NumValue(42)
    v.asNumber shouldBe Some(42.0)
  }

  it should "not extract as boolean" in {
    val v = NumValue(42)
    v.asBoolean shouldBe None
  }

  it should "not extract as task" in {
    val v = NumValue(42)
    v.asTask shouldBe None
  }

  it should "show correctly" in {
    NumValue(42).show shouldBe "42.0"
    NumValue(3.14).show shouldBe "3.14"
  }

  it should "have correct typeName" in {
    NumValue(42).typeName shouldBe "Number"
  }

  // ============================================
  // BOOL VALUE
  // ============================================

  "BoolValue" should "extract as boolean" in {
    BoolValue(true).asBoolean shouldBe Some(true)
    BoolValue(false).asBoolean shouldBe Some(false)
  }

  it should "not extract as number" in {
    BoolValue(true).asNumber shouldBe None
  }

  it should "show correctly" in {
    BoolValue(true).show shouldBe "true"
    BoolValue(false).show shouldBe "false"
  }

  it should "have correct typeName" in {
    BoolValue(true).typeName shouldBe "Boolean"
  }

// ============================================
  // TASK VALUE
  // ============================================

  "TaskValue" should "extract as task" in {
    val task: Task[Value] = TaskBuilder.task("test")(NumValue(42)).build
    val v = TaskValue(task)
    v.asTask shouldBe Some(task)
  }

  it should "not extract as number or boolean" in {
    val task: Task[Value] = TaskBuilder.task("test")(NumValue(42)).build
    val v = TaskValue(task)
    v.asNumber shouldBe None
    v.asBoolean shouldBe None
  }

  it should "show task ID" in {
    val task: Task[Value] = TaskBuilder.task("test")(NumValue(42))
      .withId(TaskId("my-task-id"))
      .build
    val v = TaskValue(task)
    v.show should include("my-task-id")
  }

  it should "have correct typeName" in {
    val task: Task[Value] = TaskBuilder.task("test")(NumValue(42)).build
    TaskValue(task).typeName shouldBe "Task"
  }

  // ============================================
  // TASK RESULT VALUE
  // ============================================

  "TaskResultValue" should "extract as task result" in {
    val result: TaskResult[Int] = Success(TaskId("t1"), 42, 1.second)
    val v = TaskResultValue(result.asInstanceOf[TaskResult[Value]])
    v.asTaskResult.isDefined shouldBe true
  }

  it should "show result message" in {
    val result: TaskResult[Int] = Success(TaskId("t1"), 42, 1.second)
    val v = TaskResultValue(result.asInstanceOf[TaskResult[Value]])
    v.show should include("succeeded")
  }

  it should "have correct typeName" in {
    val result: TaskResult[Int] = Success(TaskId("t1"), 42, 1.second)
    val v = TaskResultValue(result.asInstanceOf[TaskResult[Value]])
    v.typeName shouldBe "TaskResult"
  }

  // ============================================
  // TASK LIST VALUE
  // ============================================

  "TaskListValue" should "extract as task list" in {
    val tasks = List(
      TaskBuilder.task("t1")(1).build,
      TaskBuilder.task("t2")(2).build
    )
    val v = TaskListValue(tasks.asInstanceOf[List[Task[Value]]])
    v.asTaskList.isDefined shouldBe true
    v.asTaskList.get should have size 2
  }

  it should "show task count" in {
    val tasks = List(
      TaskBuilder.task("t1")(1).build,
      TaskBuilder.task("t2")(2).build,
      TaskBuilder.task("t3")(3).build
    )
    val v = TaskListValue(tasks.asInstanceOf[List[Task[Value]]])
    v.show should include("3 tasks")
  }

  it should "have correct typeName" in {
    TaskListValue(List.empty).typeName shouldBe "TaskList"
  }

  // ============================================
  // WORKFLOW VALUE
  // ============================================

  "WorkflowValue" should "extract as workflow" in {
    val v = WorkflowValue("myWorkflow", NumLit(42))
    v.asWorkflow shouldBe Some(("myWorkflow", NumLit(42)))
  }

  it should "show workflow name" in {
    val v = WorkflowValue("myWorkflow", NumLit(42))
    v.show should include("myWorkflow")
  }

  it should "have correct typeName" in {
    WorkflowValue("test", NumLit(1)).typeName shouldBe "Workflow"
  }

  // ============================================
  // TYPE CHECKING
  // ============================================

  "Value types" should "be mutually exclusive" in {
    // NumValue
    val num = NumValue(42)
    num.asNumber.isDefined shouldBe true
    num.asBoolean.isDefined shouldBe false
    num.asTask.isDefined shouldBe false
    num.asTaskResult.isDefined shouldBe false
    num.asTaskList.isDefined shouldBe false
    num.asWorkflow.isDefined shouldBe false

    // BoolValue
    val bool = BoolValue(true)
    bool.asNumber.isDefined shouldBe false
    bool.asBoolean.isDefined shouldBe true
    bool.asTask.isDefined shouldBe false

    // TaskValue
    val task = TaskValue(TaskBuilder.task("t")(NumValue(1)).build)
    task.asNumber.isDefined shouldBe false
    task.asBoolean.isDefined shouldBe false
    task.asTask.isDefined shouldBe true
  }
}