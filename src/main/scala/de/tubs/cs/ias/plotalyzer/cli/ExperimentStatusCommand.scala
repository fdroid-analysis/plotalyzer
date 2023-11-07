package de.tubs.cs.ias.plotalyzer.cli

import com.github.vickumar1981.stringdistance.StringDistance._
import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.applist.{AppListParser, MobileAppList}
import de.tubs.cs.ias.plotalyzer.database.entities.{
  AppAnalyzerError,
  Experiment,
  InterfaceAnalysis,
  MobileApp
}
import de.tubs.cs.ias.plotalyzer.utility.AsciiProgressBar
import scala.collection.mutable.HashMap
import spray.json.{JsArray, JsObject, JsString, JsValue}
import wvlet.log.LogSupport

object ExperimentStatusCommand extends Command with LogSupport {

  val parser: Parser = Parser("status", "show the experiment status")
    .addPositional("app-list", "mobile app list of all apps to be analyzed in the experiment")
    .addFlag("listAll", "l", "list", "list app ids")
    .addSubparser(
      Parser("all", "number of apps which are done, outstanding or failed")
        .addDefault[ParsingResult => JsValue]("func", this.showStatus)
    )
    .addSubparser(
      Parser("done", "number of apps that finished analysis")
        .addDefault[ParsingResult => JsValue]("func", this.showDone)
    )
    .addSubparser(
      Parser("outstanding", "number of apps not yet analyzed")
        .addDefault[ParsingResult => JsValue]("func", this.showOutstanding)
    )
    .addSubparser(
      Parser("failed", "number of apps that failed analysis")
        .addFlag("group", "g", "group", "group apps by failure type")
        .addOptional(
          "similarity",
          "s",
          "similar",
          Some("0.95"),
          "similarity between error messages to consider them the same, e.g. 0.5/0.8/0.99"
        )
        .addDefault[ParsingResult => JsValue]("func", this.showFailed)
    )
  private var executionCount = 0

  private def showStatus(pargs: ParsingResult): JsValue = {
    val done = showDone(pargs)
    val outstanding = showOutstanding(pargs)
    val failed = showFailed(pargs)
    JsObject(done.fields ++ outstanding.fields ++ failed.fields)
  }

  private def showDone(pargs: ParsingResult): JsObject = {
    execute(pargs) { (analyses, params) =>
      val success = analyses.filter(_.isSuccess).groupBy(_.getApp)
      val done = success.keys.filter(app => params.list.apps.map(_.bundleId).contains(app.id))
      val json = JsObject(("analyzedSuccess", JsArray(done.map(app => JsString(app.id)).toVector)))
      (done.toList, json, "analyzed successfully")
    }
  }

  private def showOutstanding(pargs: ParsingResult): JsObject = {
    execute(pargs) { (analyses, params) =>
      val analysedApps = analyses.map(_.getApp.id)
      val outstanding = params
        .list
        .apps
        .filterNot(app => analysedApps.contains(app.bundleId))
        .map(app => MobileApp(app.bundleId, app.version, "NA"))
      val entries = outstanding
        .map(app => JsObject(("id", JsString(app.id)), ("version", JsString(app.version))))
      val json = JsObject(("notAnalyzed", JsArray(entries.toVector)))
      (outstanding, json, "not analyzed yet")
    }
  }

  private def showFailed(pargs: ParsingResult): JsObject = {
    val groupByError = pargs.getValue[Boolean]("group")
    val similarity = pargs.getValue[String]("similarity").toDouble
    execute(pargs) { (analyses, params) =>
      val successIds = analyses.filter(_.isSuccess).groupBy(_.getApp).keySet.map(_.id)
      val neverSucceeded = analyses
        .filterNot(ia => successIds.contains(ia.getApp.id))
        .groupBy(_.getApp)
      val failed = neverSucceeded
        .filter(appAnalyses => params.list.apps.map(_.bundleId).contains(appAnalyses._1.id))
      var json = JsObject(
        ("analyzedFailed", JsArray(failed.map(app => JsString(app._1.id)).toVector))
      )
      if (groupByError) {
        val failGroups = groupByFailure(failed, similarity)
        val reasons = JsObject(
          failGroups.map { group =>
            group._1 -> JsArray(group._2.map(app => JsString(app.toString)).toVector)
          }
        )
        val failureReasons = Map("failureReasons" -> reasons)
        json = JsObject(json.fields ++ failureReasons)
      }
      (failed.keys.toList, json, "never analyzed successfully")
    }
  }

  private def groupByFailure(
      failed: Map[MobileApp, List[InterfaceAnalysis]],
      similarity: Double
  ): Map[String, List[MobileApp]] = {
    val failGroups: HashMap[String, List[MobileApp]] = HashMap()
    val bar = AsciiProgressBar.create("Comparing failure reasons", failed.size.toLong)
    try {
      for (app <- failed.keys) {
        for (analysis <- failed(app)) {
          val appFails = analysis.getInterfaceErrors ++ analysis.getExperimentErrors
          mostSimilarFails(failGroups, appFails, app, similarity)
        }
        bar.step()
      }
    } finally {
      bar.close()
    }
    failGroups.toMap
  }

  private def mostSimilarFails(
      failGroups: HashMap[String, List[MobileApp]],
      appFails: List[AppAnalyzerError],
      app: MobileApp,
      similarity: Double
  ): Unit = {
    for (fail <- appFails) {
      var highestSimilarity: Double = -1
      var mostSimilarFail: String = ""
      val analysisMessage = fail.getMessage.replace(app.id, "?")
      for (failMessage <- failGroups.keys) {
        val similarity = JaroWinkler.score(failMessage.slice(0, 200), analysisMessage.slice(0, 200))
        if (similarity > highestSimilarity) {
          highestSimilarity = similarity
          mostSimilarFail = failMessage
        }
      }
      if (highestSimilarity >= similarity) {
        failGroups(mostSimilarFail) = failGroups(mostSimilarFail) ++ List(app)
      } else {
        failGroups(analysisMessage) = List(app)
      }
    }
  }

  private def execute(pargs: ParsingResult)(implicit
      func: (List[InterfaceAnalysis], Params) => (List[MobileApp], JsObject, String)
  ): JsObject = {
    executionCount += 1
    val params = getParams(pargs)
    val analyses = getAnalyses(params)
    if (executionCount == 1) {
      info(s"there are ${params.list.apps.size} apps on the list")
      info(s"there are analyses for ${analyses.groupBy(_.getApp).size} apps")
    }
    val (apps, json, postfix) = func(analyses, params)
    val percent = apps.size.toFloat / params.list.apps.size * 100
    info(f"${apps.size} ($percent%2.2f %%) apps were $postfix")
    if (params.listAll) {
      apps.map(_.id).foreach(println)
    }
    json
  }

  private def getAnalyses(params: Params): List[InterfaceAnalysis] = {
    val experiment = Experiment(params.experimentId.toString)
    InterfaceAnalysis.get(experiment)
  }

  private def getParams(pargs: ParsingResult): Params = {
    val eid = pargs.getValue[String]("id").toInt
    val apps = AppListParser.read(pargs.getValue[String]("app-list"))
    val listAll = pargs.getValue[Boolean]("listAll")
    Params(eid, apps, listAll)
  }

  private case class Params(experimentId: Int, list: MobileAppList, listAll: Boolean)

}
