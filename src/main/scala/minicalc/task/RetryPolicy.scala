package minicalc.task

import scala.concurrent.duration.{DurationInt, FiniteDuration}

/**
 * Strategy for retrying failed tasks.
 *
 * Defines whether and how to retry a task after failure, including
 * the maximum number of attempts and the delay between attempts.
 */
sealed trait RetryPolicy {

  /**
   * Calculates the delay before the given retry attempt.
   *
   * @param attemptNumber The attempt number (1-indexed)
   * @return Duration to wait before this attempt
   */
  def delayForAttempt(attemptNumber: Int): FiniteDuration

  /**
   * Determines if another retry should be attempted.
   *
   * @param attemptNumber The current attempt number (1-indexed)
   * @return true if more retries are allowed, false otherwise
   */
  def shouldRetry(attemptNumber: Int): Boolean
}

/**
 * No retry policy - task runs once and fails permanently on error.
 */
case object NoRetry extends RetryPolicy {

  def delayForAttempt(attemptNumber: Int): FiniteDuration = 0.seconds

  def shouldRetry(attemptNumber: Int): Boolean = false
}

/**
 * Linear retry policy with constant delay between attempts.
 *
 * Each retry waits the same amount of time. Useful for transient
 * failures that may resolve with a simple wait.
 *
 * @param maxAttempts Maximum number of attempts (must be 1-100)
 * @param delay       Constant delay between attempts
 */
case class LinearRetry(maxAttempts: Int, delay: FiniteDuration) extends RetryPolicy {
  require(maxAttempts > 0, "maxAttempts must be positive")
  require(maxAttempts <= 100, "maxAttempts must be 100 or less")
  require(delay.length > 0, "delay must be positive")

  def shouldRetry(attemptNumber: Int): Boolean = {
    attemptNumber < maxAttempts
  }

  def delayForAttempt(attemptNumber: Int): FiniteDuration = {
    delay
  }
}

/**
 * Exponential backoff retry policy with increasing delays.
 *
 * Each retry waits exponentially longer: baseDelay * 2^(attempt-1).
 * Useful for avoiding thundering herd problems and giving systems
 * time to recover from overload.
 *
 * Example with baseDelay = 1.second:
 * - Attempt 1: 1 second
 * - Attempt 2: 2 seconds
 * - Attempt 3: 4 seconds
 * - Attempt 4: 8 seconds
 *
 * @param maxAttempts Maximum number of attempts (must be 1-25, limited to prevent overflow)
 * @param baseDelay   Initial delay, doubles with each attempt
 */
case class ExponentialRetry(maxAttempts: Int, baseDelay: FiniteDuration) extends RetryPolicy {
  require(maxAttempts > 0, "maxAttempts must be positive")
  require(maxAttempts <= 25, "maxAttempts must be 25 or less")
  require(baseDelay.length > 0, "delay must be positive")

  def shouldRetry(attemptNumber: Int): Boolean = {
    attemptNumber < maxAttempts
  }

  def delayForAttempt(attemptNumber: Int): FiniteDuration = {
    // Exponential backoff: baseDelay * 2^(attempt-1)
    baseDelay * math.pow(2, attemptNumber - 1).toLong
  }
}