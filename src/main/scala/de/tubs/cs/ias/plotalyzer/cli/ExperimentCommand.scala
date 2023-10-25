package de.tubs.cs.ias.plotalyzer.cli

import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.applist.{AppListParser, MobileAppList}
import de.tubs.cs.ias.plotalyzer.database.entities.{
  Experiment,
  InterfaceAnalysis,
  InterfaceAnalysisError,
  MobileApp
}
import spray.json.{JsArray, JsObject, JsString, JsValue}
import wvlet.log.LogSupport

object ExperimentCommand extends Command with LogSupport {

  val parser: Parser = Parser("experiment", "manage experiments")
    .addPositional("app-list", "mobile app list of all apps to be analyzed in the experiment")
    .addFlag("listAll", "l", "list", "list app ids")
    .addSubparser(
      Parser("status", "number of apps which are done, outstanding or failed")
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
    execute(pargs) { (analyses, errors, params) =>
      val errorIds = errors.map(_.getAnalysisId)
      val success = analyses.filterNot(ia => errorIds.contains(ia.getId)).groupBy(_.getApp)
      val done = success.keys.filter(app => params.list.apps.map(_.bundleId).contains(app.id))
      val json = JsObject(
        ("analyzedSuccess", JsArray(done.map(app => JsString(app.id)).toVector))
      )
      (done.toList, json, "analyzed successfully")
    }
  }

  private def showOutstanding(pargs: ParsingResult): JsObject = {
    execute(pargs) { (analyses, errors, params) =>
      val analysedApps = analyses.map(_.getApp.id)
      val outstanding = params
        .list
        .apps
        .filterNot(app => analysedApps.contains(app.bundleId))
        .map(app => MobileApp(app.bundleId, app.version, "Android"))
      val json = JsObject(
        ("notAnalyzed", JsArray(outstanding.map(app => JsString(app.id)).toVector))
      )
      (outstanding, json, "not analyzed yet")
    }
  }

  private def showFailed(pargs: ParsingResult): JsObject = {
    execute(pargs) { (analyses, errors, params) =>
      val errorIds = errors.map(_.getAnalysisId)
      val successIds = analyses
        .filterNot(ia => errorIds.contains(ia.getId))
        .groupBy(_.getApp)
        .keySet
        .map(_.id)
      val neverSucceeded = analyses
        .filterNot(ia => successIds.contains(ia.getApp.id))
        .groupBy(_.getApp)
      val failed = neverSucceeded
        .keys
        .filter(app => params.list.apps.map(_.bundleId).contains(app.id))
      val json = JsObject(("analyzedFailed", JsArray(failed.map(app => JsString(app.id)).toVector)))
      (failed.toList, json, "never analyzed successfully")
    }
  }

  private def execute(pargs: ParsingResult)(implicit
      func: (List[InterfaceAnalysis], List[InterfaceAnalysisError], Params) => (
          List[MobileApp],
          JsObject,
          String
      )
  ): JsObject = {
    executionCount += 1
    val params = getParams(pargs)
    val analyses = getAnalyses(params)
    val errors = InterfaceAnalysisError.getInterfaceAnalysisErrors(analyses.map(_.getId))
    if (executionCount == 1) {
      info(s"there are ${params.list.apps.size} apps on the list")
      info(s"there are analyses for ${analyses.groupBy(_.getApp).size} apps")
    }
    val (apps, json, postfix) = func(analyses, errors, params)
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
