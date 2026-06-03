package minicalc.task

/**
 * Task priority levels for scheduling and execution ordering.
 *
 * Implements Ordered for easy comparison and sorting. Higher priority
 * tasks should generally be executed before lower priority tasks.
 *
 * Priority levels from highest to lowest: High > Normal > Low
 */
sealed trait Priority extends Ordered[Priority] {

  /**
   * Compares this priority with another.
   *
   * @param that The priority to compare with
   * @return Positive if this > that, negative if this < that, 0 if equal
   */
  override def compare(that: Priority): Int = {
    (this, that) match {
      case (High, High)     => 0
      case (High, _)        => 1
      case (_, High)        => -1
      case (Normal, Normal) => 0
      case (Normal, Low)    => 1
      case (Low, Normal)    => -1
      case (Low, Low)       => 0
    }
  }

  /**
   * Returns the string representation of this priority.
   *
   * @return "High", "Normal", or "Low"
   */
  override def toString: String = {
    this match {
      case High   => "High"
      case Normal => "Normal"
      case Low    => "Low"
    }
  }
}

/** Highest priority level - tasks execute first. */
case object High extends Priority

/** Default priority level. */
case object Normal extends Priority

/** Lowest priority level - tasks execute last. */
case object Low extends Priority

/**
 * Companion object with factory methods and implicit ordering.
 */
object Priority {

  /**
   * Parses a string into a Priority.
   *
   * @param value String to parse ("High", "Normal", or "Low")
   * @return Some(Priority) if valid, None otherwise
   */
  def fromString(value: String): Option[Priority] = {
    value match {
      case "High"   => Some(High)
      case "Normal" => Some(Normal)
      case "Low"    => Some(Low)
      case _        => None
    }
  }

  /**
   * Implicit ordering for Priority, mapping to numeric values.
   * High = 3, Normal = 2, Low = 1 (higher number = higher priority)
   */
  implicit val ordering: Ordering[Priority] = Ordering.by {
    case High   => 3
    case Normal => 2
    case Low    => 1
  }
}