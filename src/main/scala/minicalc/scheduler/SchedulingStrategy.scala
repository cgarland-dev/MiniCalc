package minicalc.scheduler

/**
 * Strategy for ordering tasks during scheduling.
 *
 * Determines how tasks are ordered after dependency resolution.
 * Different strategies can prioritize tasks differently based on
 * application requirements.
 */
sealed trait SchedulingStrategy {

  /**
   * Human-readable name for this strategy.
   *
   * @return The strategy name
   */
  def name: String
}

/**
 * First-In-First-Out scheduling strategy.
 *
 * Tasks are executed in the order they were added, after
 * dependency constraints are satisfied. This is the simplest
 * and most predictable scheduling strategy.
 */
case object FIFOStrategy extends SchedulingStrategy {
  override def name: String = "FIFO"
}

/**
 * Priority-based scheduling strategy.
 *
 * Tasks with higher priority are executed first, after
 * dependency constraints are satisfied. Useful when some
 * tasks are more important or time-sensitive than others.
 */
case object PriorityStrategy extends SchedulingStrategy {
  override def name: String = "PRIORITY"
}