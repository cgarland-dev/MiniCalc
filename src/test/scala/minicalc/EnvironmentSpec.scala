package minicalc

import cats.effect.unsafe.implicits.global
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import minicalc.ast.{BoolValue, NumValue}
import minicalc.eval.Environment
import minicalc.ast.Value

class EnvironmentSpec extends AnyFlatSpec with Matchers {

  // Helper to create fresh environment
  def freshEnv: Environment = Environment.create.unsafeRunSync()

  // ============================================
  // CREATION
  // ============================================

  "Environment.create" should "create an environment with empty bindings" in {
    val env = freshEnv
    env.bindings shouldBe empty
  }

  // ============================================
  // ADD OPERATION
  // ============================================

  "add" should "add a new binding" in {
    val env = freshEnv.add("x", NumValue(5.0))
    env.get("x") shouldBe Some(NumValue(5.0))
  }

  it should "return a new environment without modifying the original" in {
    val env1 = freshEnv
    val env2 = env1.add("x", NumValue(5.0))

    env1.get("x") shouldBe None
    env2.get("x") shouldBe Some(NumValue(5.0))
  }

  it should "allow multiple additions" in {
    val env = freshEnv
      .add("x", NumValue(5.0))
      .add("y", NumValue(10.0))
      .add("z", BoolValue(true))

    env.get("x") shouldBe Some(NumValue(5.0))
    env.get("y") shouldBe Some(NumValue(10.0))
    env.get("z") shouldBe Some(BoolValue(true))
  }

  it should "shadow existing bindings" in {
    val env = freshEnv
      .add("x", NumValue(5.0))
      .add("x", NumValue(10.0))

    env.get("x") shouldBe Some(NumValue(10.0))
  }

  // ============================================
  // GET OPERATION
  // ============================================

  "get" should "return Some for existing bindings" in {
    val env = freshEnv.add("x", NumValue(5.0))
    env.get("x") shouldBe Some(NumValue(5.0))
  }

  it should "return None for non-existent bindings" in {
    val env = freshEnv
    env.get("x") shouldBe None
  }

  it should "be case-sensitive" in {
    val env = freshEnv.add("x", NumValue(5.0))
    env.get("X") shouldBe None
  }

  // ============================================
  // METADATA
  // ============================================

  "addMetadata" should "add metadata without affecting bindings" in {
    val env = freshEnv
      .add("x", NumValue(5.0))
      .addMetadata("key", "value")

    env.get("x") shouldBe Some(NumValue(5.0))
    env.metadata("key") shouldBe "value"
  }

  it should "return a new environment" in {
    val env1 = freshEnv
    val env2 = env1.addMetadata("key", "value")

    env1.metadata.get("key") shouldBe None
    env2.metadata.get("key") shouldBe Some("value")
  }

  it should "preserve existing metadata" in {
    val env = freshEnv
      .addMetadata("key1", "value1")
      .addMetadata("key2", "value2")

    env.metadata("key1") shouldBe "value1"
    env.metadata("key2") shouldBe "value2"
  }

  it should "preserve bindings when adding metadata" in {
    val env = freshEnv
      .add("x", NumValue(5.0))
      .addMetadata("key", "value")
      .add("y", NumValue(10.0))

    env.get("x") shouldBe Some(NumValue(5.0))
    env.get("y") shouldBe Some(NumValue(10.0))
    env.metadata("key") shouldBe "value"
  }

  // ============================================
  // IMMUTABILITY
  // ============================================

  "Environment" should "be immutable" in {
    val env1 = freshEnv
    val env2 = env1.add("x", NumValue(5.0))
    val env3 = env2.add("y", NumValue(10.0))

    // Original environments should be unchanged
    env1.bindings shouldBe empty
    env2.get("x") shouldBe Some(NumValue(5.0))
    env2.get("y") shouldBe None
    env3.get("x") shouldBe Some(NumValue(5.0))
    env3.get("y") shouldBe Some(NumValue(10.0))
  }

  // ============================================
  // COMPLEX SCENARIOS
  // ============================================

  "Environment" should "handle multiple updates correctly" in {
    val env = freshEnv
      .add("x", NumValue(1.0))
      .add("y", NumValue(2.0))
      .add("x", NumValue(10.0)) // Shadow x
      .add("z", NumValue(3.0))

    env.get("x") shouldBe Some(NumValue(10.0)) // Latest value
    env.get("y") shouldBe Some(NumValue(2.0))
    env.get("z") shouldBe Some(NumValue(3.0))
  }

  it should "work with both numeric and boolean values" in {
    val env = freshEnv
      .add("num", NumValue(42.0))
      .add("flag", BoolValue(true))

    env.get("num") shouldBe Some(NumValue(42.0))
    env.get("flag") shouldBe Some(BoolValue(true))
  }

  it should "handle variables with similar names" in {
    val env = freshEnv
      .add("x", NumValue(1.0))
      .add("x1", NumValue(2.0))
      .add("x_var", NumValue(3.0))

    env.get("x") shouldBe Some(NumValue(1.0))
    env.get("x1") shouldBe Some(NumValue(2.0))
    env.get("x_var") shouldBe Some(NumValue(3.0))
  }

  it should "work correctly when chaining operations" in {
    val result = freshEnv
      .add("a", NumValue(1.0))
      .add("b", NumValue(2.0))
      .addMetadata("created", "2024")
      .add("c", NumValue(3.0))
      .get("b")

    result shouldBe Some(NumValue(2.0))
  }

  // ============================================
  // TASK MANAGEMENT
  // ============================================

  "addTask" should "add a task to the environment" in {
    import minicalc.task.TaskBuilder
    val env = freshEnv
    val task = TaskBuilder.task[Value]("myTask")(NumValue(42)).build
    val newEnv = env.addTask("myTask", task)

    newEnv.getTask("myTask").isDefined shouldBe true
  }

  "clearTasks" should "remove all tasks" in {
    import minicalc.task.TaskBuilder
    val env = freshEnv
    val task = TaskBuilder.task[Value]("myTask")(NumValue(42)).build
    val envWithTask = env.addTask("myTask", task)
    val clearedEnv = envWithTask.clearTasks

    clearedEnv.getTask("myTask").isDefined shouldBe false
  }

  // ============================================
  // WORKFLOW MANAGEMENT
  // ============================================

  "addWorkflow" should "add a workflow to the environment" in {
    import minicalc.ast.NumLit
    val env = freshEnv
    val newEnv = env.addWorkflow("myFlow", NumLit(42))

    newEnv.getWorkflow("myFlow").isDefined shouldBe true
  }

  // ============================================
  // SETTINGS MANAGEMENT
  // ============================================

  "withSettings" should "update settings" in {
    import minicalc.repl.REPLSettings
    val env = freshEnv
    val newSettings = REPLSettings(verbose = true)
    val newEnv = env.withSettings(newSettings)

    newEnv.settings.verbose shouldBe true
  }
}