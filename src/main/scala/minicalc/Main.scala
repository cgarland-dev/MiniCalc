package minicalc

import cats.effect.{IOApp, ExitCode, IO}
import minicalc.repl.REPL

/**
 * Main entry point for the MiniCalc interpreter.
 * 
 * Starts the interactive REPL
 * To run: sbt run
 */
object Main extends IOApp {
  def run(args: List[String]): IO[ExitCode] = 
    REPL.run(args)
}