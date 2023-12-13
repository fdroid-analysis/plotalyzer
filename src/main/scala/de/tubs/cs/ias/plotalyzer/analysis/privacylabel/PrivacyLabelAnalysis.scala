package de.tubs.cs.ias.plotalyzer.analysis.privacylabel

import de.tubs.cs.ias.plotalyzer.database.entities.Experiment
import de.tubs.cs.ias.util.{FileSystemInteraction => fsi}
import spray.json.{JsArray, JsNumber, JsObject, JsValue}
import wvlet.log.LogSupport

object PrivacyLabelAnalysis extends LogSupport {

  def privacyLabelConsistency[PL <: PrivacyLabel](
      experimentId: Int,
      labelFolder: String,
      labelSuffix: String,
      labelFileChooser: (String, List[String]) => Option[String],
      labelFileParser: String => Option[PL],
      filterListIds: List[Int]
  ): JsValue = {
    // get all interfaceanalysis which succeeded for experiment, choose one per app (most recent)
    // get corresponding trafficcollection for app
    // get corresponding requests for app
    // get requestmatches for app and specified filterlist(s)
    // get privacy label of app by applying privacy label chooser and parser to files in privacyLabelFolder
    /*apply eval function  to judge if requestmatches and privacy label are consistent,
      e.g. app contacts url on easylist(_noelemhide), is also tagged with AntiFeature Ads*/
    // eval function: (List[RequestMatch], PrivacyLabel) --> PrivacyLabelConsistencyResult
    val experiment = Experiment.apply(experimentId.toString)
    val successfulAnalyses = experiment.getLatestSuccessfulAppInterfaceAnalysesWithTraffic
    val appToAnalysis = successfulAnalyses.map(analysis => analysis.getApp.id -> analysis).toMap
    val files = fsi.getFiles(labelFolder, Some(labelSuffix))
    val appToLabelFile = successfulAnalyses.map(_.getApp.id)
      .map(appId => appId -> labelFileChooser(appId, files)).toMap.filter(_._2.nonEmpty).view
      .mapValues(_.get)
    val appToLabel = appToLabelFile.mapValues(fileName => s"$labelFolder/$fileName")
      .mapValues(labelFileParser).filter(_._2.nonEmpty).mapValues(_.get).toMap
    val appToConsistencyResult = appToLabel.map { appLabel =>
      appLabel._1 ->
        PrivacyLabelConsistencyResult(appToAnalysis(appLabel._1), filterListIds, appLabel._2)
    }
    val consistent = appToConsistencyResult.filter(_._2.isConsistent).values.map(_.toJson).toVector
    val inconsistent = appToConsistencyResult.filterNot(_._2.isConsistent).values.map(_.toJson).toVector
    info(s"consistent: ${consistent.length}")
    info(s"inconsistent: ${inconsistent.length}")
    JsObject(
      "consistent" ->
        JsObject("count" -> JsNumber(consistent.length), "apps" -> JsArray(consistent)),
      "inconsistent" ->
        JsObject("count" -> JsNumber(inconsistent.length), "apps" -> JsArray(inconsistent))
    )
  }
}
