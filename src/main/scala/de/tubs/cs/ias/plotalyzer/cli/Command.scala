package de.tubs.cs.ias.plotalyzer.cli

import de.halcony.argparse.Parser
import wvlet.log.LogSupport

trait Command {
  val parser: Parser
}
