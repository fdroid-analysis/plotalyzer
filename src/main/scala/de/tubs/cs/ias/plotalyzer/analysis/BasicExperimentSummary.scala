package de.tubs.cs.ias.plotalyzer.analysis

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.Request
import de.tubs.cs.ias.plotalyzer.database.entities.{
  Experiment,
  InterfaceAnalysis,
  MobileApp
}
import spray.json.{JsNumber, JsObject, JsString, JsValue}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration.Inf

class BasicExperimentSummary(interfaceAnalysis: List[InterfaceAnalysis]) {

  val analyzedApps :: analyzedAppsNoError :: Nil = {
    val fut = Future.sequence {
      List(
        Future { interfaceAnalysis.map(_.getApp).toSet.size },
        Future {
          interfaceAnalysis.filter(_.getErrors.isEmpty).map(_.getApp).toSet.size
        }
      )
    }
    Await.result(fut, Inf)
  }

  val (requests, requestNoError) = {
    val reqs: List[Request] =
      interfaceAnalysis.flatMap(_.getTrafficCollection.flatMap(_.getRequests))
    (reqs.length, reqs.count(_.error.isEmpty))
  }

  /** {
    *   analyzedApps : <NUMBER>,
    *   analyzedAppsNoError : <NUMBER>,
    *   interfaceAnalysis : {
    *      count : <NUMBER>,
    *      noError : <NUMBER>,
    *      success : <NUMBER>,
    *      interfaces : <NUMBER>,
    *      interfacesWithScreenshot : <NUMBER>,
    *      appStats : {
    *           avg : <NUMBER>,
    *           <NUMBER> : { count : <NUMBER>, countNoErr : <NUMBER> }, ...
    *      }
    *   },
    *   requests : {
    *      count : <NUMBER>,
    *      countNoError : <NUMBER>,
    *   }
    * }
    *
    * @return a JsObject that can pretty prints as specified above
    */
  def toJson: JsValue = {
    JsObject(
      "analyzedApps" -> JsNumber(analyzedApps),
      "analyzedAppsNoError" -> JsNumber(analyzedAppsNoError),
      "interfaceAnalysis" -> JsObject(
        "count" -> JsNumber(interfaceAnalysis.length),
        "noError" -> JsNumber(interfaceAnalysis.count(_.getErrors.isEmpty)),
        "success" -> JsNumber(interfaceAnalysis.count(_.getSuccess)),
        "interfaces" -> JsString("N/A"),
        "interfacesWithScreenshot" -> JsString("NA"),
        "appStats" -> getAppStats
      ),
      "requests" -> JsObject(
        "count" -> JsNumber(requests),
        "countNoError" -> JsNumber(requestNoError)
      )
    )
  }

  private def getAppStats: JsObject = {
    val base = interfaceAnalysis.groupBy(_.getApp)
    val analysisCountPerApp: List[(MobileApp, Int)] =
      base.map(pair => (pair._1, pair._2.length)).toList
    val analysisNoErrorCountPerApp: List[(MobileApp, Int)] =
      base.map(pair => (pair._1, pair._2.count(_.getErrors.isEmpty))).toList
    val appCountPerAnalysisCount: Map[Int, Int] =
      analysisCountPerApp.groupBy(_._2).map(tuple => (tuple._1, tuple._2.length))
    val appCountNoErrorPerAnalysisCount: Map[Int, Int] =
      analysisNoErrorCountPerApp.groupBy(_._2).map(tuple => (tuple._1, tuple._2.length))
    JsObject(
      "avgAnalysisCountPerApp" -> JsNumber(analysisCountPerApp.map(_._2).sum.toDouble / analysisCountPerApp.length),
      "avgAnalysisCountNoErrorPerApp" -> JsNumber(
        analysisNoErrorCountPerApp.map(_._2).sum.toDouble / analysisCountPerApp.length
      ),
      "analysisCountPerApp" -> JsObject(
        (appCountPerAnalysisCount.keySet ++ appCountNoErrorPerAnalysisCount.keySet).map { key =>
          key.toString -> JsObject(
            "count" -> JsNumber(appCountPerAnalysisCount.getOrElse(key, 0)),
            "countNoError" -> JsNumber(appCountNoErrorPerAnalysisCount.getOrElse(key, 0))
          )
        }.toMap
      )
    )
  }

}

object BasicExperimentSummary {

  def apply(experiment: Experiment)(implicit
      database: Database
  ): BasicExperimentSummary =
    new BasicExperimentSummary(InterfaceAnalysis.get(experiment))

}
