package de.tubs.cs.ias.plotalyzer.json

import spray.json.{DefaultJsonProtocol, RootJsonFormat}

case class AppRequestComparison(
  appId: String,
  thisTrafficCollectionId: Int,
  thatTrafficCollectionId: Int,
  requestsBoth: List[RequestEquivalence],
  requestsThisOnly: List[RequestEquivalence],
  requestsThatOnly: List[RequestEquivalence],
  jaccardIndex: Double
)

case class RequestEquivalence(
    url: String,
    requestIds: List[Int],
)

object AppRequestComparisonJsonProtocol extends DefaultJsonProtocol {
  implicit val RequestComparisonCategoryJsonFormat: RootJsonFormat[RequestEquivalence] = {
    jsonFormat2(RequestEquivalence)
  }

  implicit val AppRequestComparisonJsonFormat: RootJsonFormat[AppRequestComparison] = {
    jsonFormat7(AppRequestComparison)
  }
}
