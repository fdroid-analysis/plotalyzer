package de.tubs.cs.ias.plotalyzer.cli

import de.halcony.argparse.Parser

trait Command {
  val parser: Parser
}
