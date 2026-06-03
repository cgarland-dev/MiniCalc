package minicalc.repl

/**
 * Configuration settings for the REPL.
 *
 * These settings control REPL behavior and can be modified
 * at runtime using the :set command.
 *
 * @param continueOnError If true, continue executing tasks even when some fail
 * @param verbose         If true, show additional debug information
 * @param showTiming      If true, show execution time for operations
 */
case class REPLSettings(
    continueOnError: Boolean = false,
    verbose: Boolean = false,
    showTiming: Boolean = false
) {

  /**
   * Creates a formatted string showing all current settings.
   *
   * @return Multi-line string displaying setting names and values
   */
  def show: String = {
    s"""REPL Settings:
       |  continue-on-error = $continueOnError
       |  verbose           = $verbose
       |  show-timing       = $showTiming""".stripMargin
  }
}

/**
 * Companion object with default settings.
 */
object REPLSettings {

  /** Default settings - all features disabled */
  val default: REPLSettings = REPLSettings()
}