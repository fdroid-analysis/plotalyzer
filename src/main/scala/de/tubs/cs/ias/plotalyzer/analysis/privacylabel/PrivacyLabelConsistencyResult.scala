package de.tubs.cs.ias.plotalyzer.analysis.privacylabel

import de.tubs.cs.ias.plotalyzer.database.entities.InterfaceAnalysis
import de.tubs.cs.ias.plotalyzer.database.entities.adblock.{FilterList, RequestMatch}
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import spray.json.{JsArray, JsNumber, JsObject, JsString}
import wvlet.log.LogSupport

case class PrivacyLabelConsistencyResult[PL <: PrivacyLabel](
    interfaceAnalysis: InterfaceAnalysis,
    filterListIds: List[Int],
    privacyLabel: PL
) extends LogSupport {

  private val requests = interfaceAnalysis
    .getTrafficCollections
    .flatMap(_.getRequests)
    .filter(_.error.isEmpty)
  private val allRequestMatches = RequestMatch
    .getRequestMatches(requests.map(_.id), filterListIds)
    .filter(_.isMatch)
  private val inconsistentRequestIds: List[Int] =
    allRequestMatches
      .map(rm => rm.requestId -> privacyLabel.isConsistent(rm))
      .toMap
      .filter(_._2 == false)
      .keys
      .toList
  def getInconsistentRequestIds: List[Int] = inconsistentRequestIds

  def toJson: JsObject = {
    val appId = interfaceAnalysis.getApp.id
    val inconsistentRequests = Request.getRequests(inconsistentRequestIds)
    val allFilterLists = FilterList.getAll

    val inconsistentRequestFilterLists =
      inconsistentRequestIds
        .map { requestId =>
          val requestMatchIds = allRequestMatches
            .filter(rm => rm.requestId == requestId)
            .map(_.listId)
          val filterLists = allFilterLists.filter(list => requestMatchIds.contains(list.id))
          requestId -> filterLists
        }
        .toMap

    val inconsistentRequestsJson =
      inconsistentRequests
        .map { request =>
          val filterListNames = inconsistentRequestFilterLists(request.id)
            .map(_.name)
            .mkString(", ")

          val path =
            if (request.path == null)
              ""
            else
              request.path
          JsObject(
            "requestId" -> JsNumber(request.id),
            "host" -> JsString(request.host),
            "path" -> JsString(path),
            "filterLists" -> JsString(filterListNames)
          )
        }
        .toVector

    val recommendedAntiFeatures = inconsistentRequestFilterLists
      .values
      .flatten
      .flatMap(list => FDroidPrivacyLabel.listIdToLabel(list.id))
      .toSet
      .map[JsString](antiFeature => JsString(antiFeature))
      .toVector

    JsObject(
      "appId" -> JsString(appId),
      "inconsistentRequests" -> JsArray(inconsistentRequestsJson),
      "privacyLabel" -> privacyLabel.toJson,
      "recommendedAntiFeatures" -> JsArray(recommendedAntiFeatures)
    )
  }

  def isConsistent: Boolean = inconsistentRequestIds.isEmpty
}
