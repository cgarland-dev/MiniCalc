package minicalc.task

import java.util.UUID
import scala.util.matching.Regex

/**
 * Unique identifier for a task.
 *
 * Uses AnyVal for zero-cost abstraction - no runtime object allocation.
 * Task IDs must be non-empty and can contain letters, numbers, hyphens,
 * underscores, and dots.
 *
 * @param value The string value of this ID
 */
case class TaskId(value: String) extends AnyVal {

  /**
   * Checks if this ID matches the valid pattern.
   *
   * @return true if the ID is valid
   */
  def isValid: Boolean = TaskId.isValid(value)

  /**
   * Returns a sanitized version of this ID.
   *
   * Converts to lowercase and replaces invalid characters.
   *
   * @return A new TaskId with sanitized value
   */
  def sanitized: TaskId = TaskId(TaskId.sanitize(value))

  /**
   * Creates a related task ID by appending a suffix.
   *
   * @param suffix The suffix to append
   * @return A new TaskId like "original-suffix"
   */
  def withSuffix(suffix: String): TaskId = TaskId(s"$value-$suffix")

  /**
   * Creates a child task ID using dot notation.
   *
   * @param childName The child name
   * @return A new TaskId like "parent.child"
   */
  def child(childName: String): TaskId = TaskId(s"$value.$childName")
}

/**
 * Companion object with factory methods and validation utilities.
 */
object TaskId {

  /** Valid task ID pattern: letters, numbers, hyphens, underscores, dots */
  private val ValidPattern: Regex = "^[a-zA-Z0-9_.-]+$".r

  /**
   * Generates a unique task ID using UUID.
   *
   * @return A TaskId like "task-550e8400-e29b-41d4-a716-446655440000"
   */
  def generate(): TaskId = {
    TaskId(s"task-${UUID.randomUUID().toString}")
  }

  /**
   * Generates a unique task ID with a custom prefix.
   *
   * @param prefix The prefix to use (will be sanitized)
   * @return A TaskId like "prefix-a1b2c3d4"
   */
  def generate(prefix: String): TaskId = {
    TaskId(s"${sanitize(prefix)}-${UUID.randomUUID().toString.take(8)}")
  }

  /**
   * Creates a task ID from a name, sanitizing it.
   *
   * Useful for creating IDs from user-provided names.
   *
   * @param name The name to convert
   * @return A sanitized TaskId
   */
  def fromName(name: String): TaskId = {
    TaskId(sanitize(name))
  }

  /**
   * Validates a string as a potential task ID.
   *
   * @param str The string to validate
   * @return true if the string would be a valid task ID
   */
  def isValid(str: String): Boolean = {
    str.nonEmpty && ValidPattern.matches(str)
  }

  /**
   * Sanitizes a string to make it a valid task ID.
   *
   * Transformations:
   * - Convert to lowercase
   * - Replace spaces with hyphens
   * - Remove invalid characters
   * - Collapse multiple hyphens
   * - Ensure non-empty (defaults to "task")
   *
   * @param str The string to sanitize
   * @return A valid task ID string
   */
  def sanitize(str: String): String = {
    val cleaned = str
      .toLowerCase
      .replaceAll("\\s+", "-")        // Spaces to hyphens
      .replaceAll("[^a-z0-9_.-]", "") // Remove invalid chars
      .replaceAll("-+", "-")          // Collapse multiple hyphens
      .stripPrefix("-")
      .stripSuffix("-")

    if (cleaned.isEmpty) "task" else cleaned
  }

  /** Implicit ordering for sorting task IDs alphabetically. */
  implicit val ordering: Ordering[TaskId] = Ordering.by(_.value)
}