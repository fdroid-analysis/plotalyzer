package de.tubs.cs.ias.plotalyzer.json

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

case class ExodusTrackerIndex(trackers: List[ExodusIndexTracker])

case class ExodusIndexTracker(
  name: String,
  category: List[String],
  codeSignature: Option[String],
  networkSignature: Option[String],
  website: String,
  documentation: List[String],
  isInExodus: Option[Boolean]
)

object ExodusTrackerIndexJsonProtocol extends DefaultJsonProtocol {
  implicit val exodusTrackerFormat: RootJsonFormat[ExodusIndexTracker] = {
    jsonFormat7(ExodusIndexTracker)
  }

  implicit val exodusTrackerIndexFormat: RootJsonFormat[ExodusTrackerIndex] = {
    jsonFormat1(ExodusTrackerIndex)
  }
}
