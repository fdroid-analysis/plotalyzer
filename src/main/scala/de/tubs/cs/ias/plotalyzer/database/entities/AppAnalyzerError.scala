package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.utility.StackTrace

trait AppAnalyzerError {
  def getMessage: String
  def getStackTrace: StackTrace
}
