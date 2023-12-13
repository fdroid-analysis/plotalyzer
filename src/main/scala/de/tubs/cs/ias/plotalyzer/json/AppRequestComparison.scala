package de.tubs.cs.ias.plotalyzer.json

import de.tubs.cs.ias.plotalyzer.database.entities.adblock.RequestMatch
import scala.annotation.unused
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

class AppRequestComparison(
    val appId: String,
    val thisTrafficCollectionId: Int,
    val thatTrafficCollectionId: Int,
    val requestsBoth: List[RequestEquivalence],
    val requestsThisOnly: List[RequestEquivalence],
    val requestsThatOnly: List[RequestEquivalence]
) extends Product10[String, Int, Int, List[RequestEquivalence], List[RequestEquivalence], List[RequestEquivalence], Double, Int, Int, Int] {
  val jaccardIndex: Double =
    requestsBoth.size.toDouble / (requestsThisOnly.size + requestsBoth.size + requestsThatOnly.size)
  val requestsBothTrackingCount: Int = getRequestsTrackingCount(requestsBoth)
  val requestsThisOnlyTrackingCount: Int = getRequestsTrackingCount(requestsThisOnly)
  val requestsThatOnlyTrackingCount: Int = getRequestsTrackingCount(requestsThatOnly)

  private def getRequestsTrackingCount(requests: List[RequestEquivalence]): Int = {
    val requestIds = requests.map(_.requestIds.head)
    RequestMatch.getRequestMatches(requestIds, matchIsIn = List(true)).size
  }

  override def _1: String = appId

  override def _2: Int = thisTrafficCollectionId

  override def _3: Int = thatTrafficCollectionId

  override def _4: List[RequestEquivalence] = requestsBoth

  override def _5: List[RequestEquivalence] = requestsThisOnly

  override def _6: List[RequestEquivalence] = requestsThatOnly

  override def _7: Double = jaccardIndex

  override def _8: Int = requestsBothTrackingCount

  override def _9: Int = requestsThisOnlyTrackingCount

  override def _10: Int = requestsThatOnlyTrackingCount

  override def canEqual(that: Any): Boolean = that.getClass == AppRequestComparison.getClass
}

object AppRequestComparison {
  def create(
    appId: String,
    thisTc: Int,
    thatTc: Int,
    rBoth: List[RequestEquivalence],
    rThis: List[RequestEquivalence],
    rThat: List[RequestEquivalence],
    @unused jc: Double,
    @unused rBothT: Int,
    @unused rThisT: Int,
    @unused rThatT: Int
  ): AppRequestComparison = {
    new AppRequestComparison(appId, thisTc, thatTc, rBoth, rThis, rThat)
  }
}

case class RequestEquivalence(url: String, requestIds: List[Int])

object AppRequestComparisonJsonProtocol extends DefaultJsonProtocol {
  implicit val RequestEquivalenceJsonFormat: RootJsonFormat[RequestEquivalence] = {
    jsonFormat2(RequestEquivalence)
  }

  implicit val AppRequestComparisonJsonFormat: RootJsonFormat[AppRequestComparison] = {
    jsonFormat(
      AppRequestComparison.create,
      "appId",
      "thisTrafficCollectionId",
      "thatTrafficCollectionId",
      "requestsBoth",
      "requestsThisOnly",
      "requestsThatOnly",
      "jaccardIndex",
      "requestsBothTrackingCount",
      "requestsThisOnlyTrackingCount",
      "requestsThatOnlyTrackingCount"
    )
  }
}
