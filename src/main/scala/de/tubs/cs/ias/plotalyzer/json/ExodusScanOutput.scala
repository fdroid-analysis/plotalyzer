package de.tubs.cs.ias.plotalyzer.json

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

case class ExodusOutput(
    application: ExodusApp,
    trackers: List[ExodusTracker]
)

case class ExodusApp(
    handle: String,
    version_code: String,
    libraries: List[String]
)

case class ExodusTracker(
    id: Int,
    name: String
)

object ExodusOutputJsonProtocol extends DefaultJsonProtocol {
  implicit val exodusAppFormat: RootJsonFormat[ExodusApp] = {
    jsonFormat3(ExodusApp)
  }

  implicit val exodusTracker: RootJsonFormat[ExodusTracker] = {
    jsonFormat2(ExodusTracker)
  }

  implicit val exodusOutputFormat: RootJsonFormat[ExodusOutput] = {
    jsonFormat2(ExodusOutput)
  }
}