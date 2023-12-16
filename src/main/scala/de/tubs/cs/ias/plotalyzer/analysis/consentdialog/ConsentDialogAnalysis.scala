package de.tubs.cs.ias.plotalyzer.analysis.consentdialog

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.adblock.{FilterList, RequestMatch}
import de.tubs.cs.ias.plotalyzer.database.entities.{Experiment, InterfaceAnalysis}
import spray.json.{JsArray, JsBoolean, JsNull, JsNumber, JsObject, JsString, JsValue}
import wvlet.log.LogSupport
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration.Inf
import scala.concurrent.{Await, Future}

case class ConsentDialogAnalysis(
    analysis: List[InterfaceAnalysis],
    conf: ConsentDialogAnalysisConfig
) extends LogSupport {

  private val appAnalysis: List[AppConsentDialogAnalysis] = {
    info(s"starting consent dialog analysis for ${analysis.length} analysis")
    val future = Future.sequence {
      val grouped = analysis.groupBy(_.getApp).toList
      info(s"we have analysis sets for ${grouped.length} different apps")
      grouped.map(set => Future(new AppConsentDialogAnalysis(set._2, conf)))
    }
    Await.result(future, Inf)
  }

  def getFinishedAnalysis: List[AppConsentDialogAnalysis] = appAnalysis

  def getFailedAnalysisIds: List[Int] = {
    appAnalysis.filter(_.getIssues.nonEmpty).flatMap(_.getAnalysisIds)
  }

  /** generate the JsObect that pretty prints the result json
    *
    * {
    *    _count : <NUMBER>,
    *    _success : <NUMBER>,
    *    _sussapps : {
    *      <APPID> : <STRING>,...
    *     }
    *    errorSummary : {
    *      <ERROR> : <NUMBER>, ...
    *    }
    *    highLevelSummary : {
    *      <DIALOGTYPE> : <NUMBER>,...
    *    },
    *    highLevelDialogSummary : {
    *      <HIGHLEVELDIALOGSUMMARY>
    *    }
    *    perAppResults : {
    *      appId : <APPCOSNENTDIALOGANALYSIS>,....
    *    }
    * }
    *
    * @return the JsObject that pretty prints to the above specifications
    */
  def toJson: JsObject = {
    JsObject(
      "_count" -> JsNumber(appAnalysis.length),
      "_success" -> JsNumber(appAnalysis.count(_.isValidConsentDialogMeasurement)),
      "_sussapps" -> getSussApps,
      "errorSummary" -> JsObject(getErrorSummary.map(elem => elem._1 -> JsNumber(elem._2))),
      "highLevelSummary" -> getHighLevelSummaryJson(),
      "highLevelDialogSummary" -> getHighLevelDialogSummary(),
      "trafficSummary" -> getTrafficSummary,
      "perAppresults" -> JsObject(appAnalysis.map(elem => elem.getAppIdString -> elem.toJson).toMap)
    )
  }

  private def getErrorSummary: Map[String, Int] = {
    appAnalysis.flatMap(_.getIssues).groupBy(elem => elem).map(elem => elem._1 -> elem._2.length)
  }

  private def getSussApps: JsObject = {
    val suss = List(ConsentDialogAnalysisIssues.NONE_WITH_ACCEPT_REJECT_ANALYSIS)
    JsObject(
      appAnalysis
        .map { analysis =>
          analysis.getIssues.find(elem => suss.contains(elem)) match {
            case Some(value) =>
              Some(analysis.getApp.id -> JsString(value))
            case None =>
              None
          }
        }
        .filter(_.nonEmpty)
        .map(_.get)
        .toMap
    )
  }

  private def getTrafficSummary: JsObject = {
    val successfulAnalyses = appAnalysis.filter(_.isValidConsentDialogMeasurement)
    val anyTraffic = successfulAnalyses.filter(analysis =>
      analysis
        .getInterfaceAnalysis
        .find(_.getDescription == "Simple Traffic Collection")
        .get // validDialogMeasurement has traffic collection
        .getTrafficCollections.flatMap(_.getRequests).nonEmpty
    )
    val filterLists = List(
      FilterList.Lists.easylist.id,
      FilterList.Lists.easylist_noelemhide.id,
      FilterList.Lists.easyprivacy.id,
      FilterList.Lists.peterlowe.id
    )
    val matchedTraffic = anyTraffic.filter(analysis => {
      val requestIds = analysis
        .getInterfaceAnalysis
        .find(_.getDescription == "Simple Traffic Collection")
        .get
        .getTrafficCollections
        .flatMap(_.getRequests)
        .map(_.id)
      val matches = RequestMatch.getRequestMatches(requestIds, filterLists, matchIsIn = List(true))
      matches.nonEmpty
    })

    val noTraffic: List[AppConsentDialogAnalysis] = successfulAnalyses.diff(anyTraffic)
    val unmatchedTraffic: List[AppConsentDialogAnalysis] = anyTraffic.diff(matchedTraffic)

    val getSummary: List[AppConsentDialogAnalysis] => JsObject = { appAnalyses =>
      val summary = Map(
        ("highLevelSummary", getHighLevelSummaryJson(appAnalyses)),
        ("highLevelDialogSummary", getHighLevelDialogSummary(appAnalyses, withAppIds = true)),
        ("count", JsNumber(appAnalyses.size)),
        ("appIds", toAppIdsJson(appAnalyses))
        )
      JsObject(summary)
    }
    JsObject(
      "none" -> getSummary(noTraffic),
      "unmatched" -> getSummary(unmatchedTraffic),
      "matched" -> getSummary(matchedTraffic)
    )
  }

  private def getHighLevelSummaryJson(
      appAnalyses: List[AppConsentDialogAnalysis] = this.appAnalysis
  ): JsValue = {
    val highLevelSummary = getHighLevelSummary(appAnalyses)
    JsObject(
      highLevelSummary.map(dialogType_analyses =>
        dialogType_analyses._1 ->
          JsObject(
            ("count", JsNumber(dialogType_analyses._2.size)),
            ("appIds", toAppIdsJson(dialogType_analyses._2))
          )
      )
    )
  }

  private def getHighLevelSummary(
      appAnalysis: List[AppConsentDialogAnalysis]
  ): Map[String, List[AppConsentDialogAnalysis]] = {
    appAnalysis.filter(_.isValidConsentDialogMeasurement).groupBy(_.getConsentDialogType)
  }

  private def getHighLevelDialogSummary(
      appAnalysis: List[AppConsentDialogAnalysis] = this.appAnalysis,
      withAppIds: Boolean = false
  ): JsValue = {
    val noErrorAnalysis = appAnalysis
      .filter(ana => ana.isValidConsentDialogMeasurement && ana.getConsentDialogType == "Dialog")

    class JsValueAsBool(v: JsValue) {
      def isTrue: Boolean = v match {
        case boolean: JsBoolean => if (boolean.value) true else false
        case JsNull => false
        case _ => throw new RuntimeException("expected JsNull or JsBoolean")
      }
    }
    implicit def convert: JsValue => JsValueAsBool = value => new JsValueAsBool(value)
    def count(
        analysis: List[AppConsentDialogAnalysis],
        func: AppConsentDialogAnalysis => JsValue
    ): Int = {
      analysis
        .map { ana => if (func(ana).isTrue) 1 else 0 }
        .sum
    }

    val acceptBaseSummary = Map(
      "exists" -> JsNumber(noErrorAnalysis.count(_.hasAccept)),
      "clear" -> JsNumber(noErrorAnalysis.count(_.hasUniqueClearAccept)),
      "equivocal" -> JsNumber(count(noErrorAnalysis, _.getAcceptEquivocalJson)),
      "larger" -> JsNumber(count(noErrorAnalysis, _.getAcceptLargerJson)),
      "highlighted" -> JsNumber(count(noErrorAnalysis, _.getAcceptHighlightedJson))
    )
    val acceptAppIds = Map(
      "apps" ->
        JsObject(
          "exists" -> toAppIdsJson(noErrorAnalysis.filter(_.hasAccept)),
          "clear" -> toAppIdsJson(noErrorAnalysis.filter(_.hasUniqueClearAccept)),
          "equivocal" -> toAppIdsJson(noErrorAnalysis.filter(_.getAcceptEquivocalJson.isTrue)),
          "larger" -> toAppIdsJson(noErrorAnalysis.filter(_.getAcceptLargerJson.isTrue)),
          "highlighted" -> toAppIdsJson(noErrorAnalysis.filter(_.getAcceptHighlightedJson.isTrue))
        )
    )
    val rejectBaseSummary = Map(
      "exists" -> JsNumber(noErrorAnalysis.count(_.hasReject)),
      "clear" -> JsNumber(noErrorAnalysis.count(_.hasUniqueClearReject)),
      "equivocal" -> JsNumber(count(noErrorAnalysis, _.getRejectEquivocalJson)),
      "closesAfter" -> JsNumber(count(noErrorAnalysis, _.getRejectStopsAppJson))
    )
    val rejectAppIds = Map(
      "apps" ->
        JsObject(
          "exists" -> toAppIdsJson(noErrorAnalysis.filter(_.hasReject)),
          "clear" -> toAppIdsJson(noErrorAnalysis.filter(_.hasUniqueClearReject)),
          "equivocal" -> toAppIdsJson(noErrorAnalysis.filter(_.getRejectEquivocalJson.isTrue)),
          "closesAfter" -> toAppIdsJson(noErrorAnalysis.filter(_.getRejectStopsAppJson.isTrue))
        )
    )

    var acceptJson: Map[String, JsValue] = acceptBaseSummary
    var rejectJson: Map[String, JsValue] = rejectBaseSummary
    if (withAppIds) {
      acceptJson = acceptBaseSummary ++ acceptAppIds
      rejectJson = rejectBaseSummary ++ rejectAppIds
    }

    JsObject("accept" -> JsObject(acceptJson), "reject" -> JsObject(rejectJson))
  }

  private def toAppIdsJson(analyses: Seq[AppConsentDialogAnalysis]): JsArray = {
    JsArray(analyses.map(_.getApp.id).map(appId => JsString(appId)).toVector)
  }

}

object ConsentDialogAnalysis {

  def apply(experiment: Experiment, config: String, only: Set[String] = Set())(implicit
      database: Database
  ): ConsentDialogAnalysis = {
    val analysis = InterfaceAnalysis
      .get(experiment)
      .filter(ana => only.isEmpty || only.contains(ana.getApp.toString))
    new ConsentDialogAnalysis(analysis, ConsentDialogAnalysisConfig.apply(config))
  }

}
