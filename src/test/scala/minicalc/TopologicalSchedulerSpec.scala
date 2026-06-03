package minicalc.scheduler

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import minicalc.task._

/**
 * Tests for the TopologicalScheduler.
 */
class TopologicalSchedulerSpec extends AnyFlatSpec with Matchers {

  val scheduler = new TopologicalScheduler()

  // Helper to create simple tasks
  def makeTask(
      name: String,
      priority: Priority = Normal,
      deps: Set[TaskId] = Set.empty
  ): Task[Int] = {
    TaskBuilder.task(name)(42)
      .withId(TaskId(name))
      .withPriority(priority)
      .dependsOn(deps.toSeq: _*)
      .build
  }

  // ============================================
  // BASIC SCHEDULING
  // ============================================

  "TopologicalScheduler" should "schedule independent tasks" in {
    val tasks = List(
      makeTask("a"),
      makeTask("b"),
      makeTask("c")
    )
    val result = scheduler.schedule(tasks, FIFOStrategy).unsafeRunSync()

    result.isRight shouldBe true
    result.toOption.get should have size 3
  }

  it should "schedule single task" in {
    val tasks = List(makeTask("single"))
    val result = scheduler.schedule(tasks, FIFOStrategy).unsafeRunSync()

    result.isRight shouldBe true
    result.toOption.get should have size 1
  }

  it should "handle empty task list" in {
    val result = scheduler.schedule(List.empty[Task[Int]], FIFOStrategy).unsafeRunSync()

    result.isRight shouldBe true
    result.toOption.get shouldBe empty
  }

  // ============================================
  // DEPENDENCY ORDERING
  // ============================================

  it should "order tasks by dependencies" in {
    val tasks = List(
      makeTask("c", deps = Set(TaskId("b"))),
      makeTask("b", deps = Set(TaskId("a"))),
      makeTask("a")
    )
    val result = scheduler.schedule(tasks, FIFOStrategy).unsafeRunSync()

    result.isRight shouldBe true
    val ordered = result.toOption.get
    val aIndex = ordered.indexWhere(_.id == TaskId("a"))
    val bIndex = ordered.indexWhere(_.id == TaskId("b"))
    val cIndex = ordered.indexWhere(_.id == TaskId("c"))

    aIndex should be < bIndex
    bIndex should be < cIndex
  }

  it should "handle multiple dependencies" in {
    val tasks = List(
      makeTask("d", deps = Set(TaskId("b"), TaskId("c"))),
      makeTask("c", deps = Set(TaskId("a"))),
      makeTask("b", deps = Set(TaskId("a"))),
      makeTask("a")
    )
    val result = scheduler.schedule(tasks, FIFOStrategy).unsafeRunSync()

    result.isRight shouldBe true
    val ordered = result.toOption.get
    val aIndex = ordered.indexWhere(_.id == TaskId("a"))
    val bIndex = ordered.indexWhere(_.id == TaskId("b"))
    val cIndex = ordered.indexWhere(_.id == TaskId("c"))
    val dIndex = ordered.indexWhere(_.id == TaskId("d"))

    aIndex should be < bIndex
    aIndex should be < cIndex
    bIndex should be < dIndex
    cIndex should be < dIndex
  }

  // ============================================
  // CYCLE DETECTION
  // ============================================

  it should "detect simple cycle (A -> B -> A)" in {
    val tasks = List(
      makeTask("a", deps = Set(TaskId("b"))),
      makeTask("b", deps = Set(TaskId("a")))
    )
    val result = scheduler.schedule(tasks, FIFOStrategy).unsafeRunSync()

    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[CircularDependency]
  }

  it should "detect longer cycle (A -> B -> C -> A)" in {
    val tasks = List(
      makeTask("a", deps = Set(TaskId("c"))),
      makeTask("b", deps = Set(TaskId("a"))),
      makeTask("c", deps = Set(TaskId("b")))
    )
    val result = scheduler.schedule(tasks, FIFOStrategy).unsafeRunSync()

    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[CircularDependency]
  }

  it should "detect self-dependency cycle" in {
    val tasks = List(
      makeTask("a", deps = Set(TaskId("a")))
    )
    val result = scheduler.schedule(tasks, FIFOStrategy).unsafeRunSync()

    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[CircularDependency]
  }

  it should "include cycle path in error" in {
    val tasks = List(
      makeTask("a", deps = Set(TaskId("b"))),
      makeTask("b", deps = Set(TaskId("a")))
    )
    val result = scheduler.schedule(tasks, FIFOStrategy).unsafeRunSync()

    result.isLeft shouldBe true
    val error = result.left.toOption.get.asInstanceOf[CircularDependency]
    error.cycle should contain(TaskId("a"))
    error.cycle should contain(TaskId("b"))
  }

  // ============================================
  // SCHEDULING STRATEGIES
  // ============================================

  it should "apply FIFO strategy (maintains order)" in {
    val tasks = List(
      makeTask("a"),
      makeTask("b"),
      makeTask("c")
    )
    val result = scheduler.schedule(tasks, FIFOStrategy).unsafeRunSync()

    result.isRight shouldBe true
    val ordered = result.toOption.get.map(_.id.value)
    ordered shouldBe List("a", "b", "c")
  }

  it should "apply Priority strategy (sorts by priority)" in {
    val tasks = List(
      makeTask("low", priority = Low),
      makeTask("high", priority = High),
      makeTask("normal", priority = Normal)
    )
    val result = scheduler.schedule(tasks, PriorityStrategy).unsafeRunSync()

    result.isRight shouldBe true
    val ordered = result.toOption.get.map(_.id.value)
    ordered shouldBe List("high", "normal", "low")
  }

  it should "respect dependencies even with Priority strategy" in {
    val tasks = List(
      makeTask("high", priority = High, deps = Set(TaskId("low"))),
      makeTask("low", priority = Low)
    )
    val result = scheduler.schedule(tasks, PriorityStrategy).unsafeRunSync()

    result.isRight shouldBe true
    val ordered = result.toOption.get.map(_.id.value)
    val lowIndex = ordered.indexOf("low")
    val highIndex = ordered.indexOf("high")
    lowIndex should be >= 0
    highIndex should be >= 0
  }

// ============================================
  // UNRESOLVABLE DEPENDENCIES
  // ============================================

  it should "return error for missing dependencies" in {
      val tasks = List(
        makeTask("a", deps = Set(TaskId("nonexistent")))
      )
      val result = scheduler.schedule(tasks, FIFOStrategy).unsafeRunSync()
      
      result.isLeft shouldBe true
      result.left.toOption.get.isInstanceOf[UnresolvableDependencies] shouldBe true
    }

  // ============================================
  // DETECT CYCLES DIRECTLY
  // ============================================

  "detectCycles" should "return None for acyclic graph" in {
    val tasks = List(
      makeTask("c", deps = Set(TaskId("b"))),
      makeTask("b", deps = Set(TaskId("a"))),
      makeTask("a")
    )
    scheduler.detectCycles(tasks) shouldBe None
  }

  it should "return Some(cycle) for cyclic graph" in {
    val tasks = List(
      makeTask("a", deps = Set(TaskId("b"))),
      makeTask("b", deps = Set(TaskId("a")))
    )
    val result = scheduler.detectCycles(tasks)
    result.isDefined shouldBe true
    result.get should not be empty
  }

  // ============================================
  // RESOLVE DEPENDENCIES DIRECTLY
  // ============================================

  "resolveDependencies" should "return tasks in topological order" in {
    val tasks = List(
      makeTask("c", deps = Set(TaskId("b"))),
      makeTask("b", deps = Set(TaskId("a"))),
      makeTask("a")
    )
    val result = scheduler.resolveDependencies(tasks)

    result.isRight shouldBe true
    val ordered = result.toOption.get
    val aIndex = ordered.indexWhere(_.id == TaskId("a"))
    val bIndex = ordered.indexWhere(_.id == TaskId("b"))
    val cIndex = ordered.indexWhere(_.id == TaskId("c"))

    aIndex should be < bIndex
    bIndex should be < cIndex
  }

  it should "return error for circular dependencies" in {
    val tasks = List(
      makeTask("a", deps = Set(TaskId("b"))),
      makeTask("b", deps = Set(TaskId("a")))
    )
    val result = scheduler.resolveDependencies(tasks)

    result.isLeft shouldBe true
    result.left.toOption.get shouldBe a[CircularDependency]
  }
}