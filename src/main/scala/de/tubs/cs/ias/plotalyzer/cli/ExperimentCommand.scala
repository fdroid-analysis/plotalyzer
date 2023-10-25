package de.tubs.cs.ias.plotalyzer.cli

import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.applist.{AppListParser, MobileAppList}
import de.tubs.cs.ias.plotalyzer.database.entities.{Experiment, InterfaceAnalysis}
import spray.json.{JsNull, JsValue}
import wvlet.log.LogSupport

object ExperimentCommand extends Command with LogSupport {

  val parser: Parser = Parser("experiment", "manage experiments")
    .addPositional("app-list", "mobile app list of all apps to be analyzed in the experiment")
    .addFlag("listAll", "l", "list", "list app ids").addSubparser(
      Parser("status", "number of apps which are done, outstanding or failed")
        .addDefault[ParsingResult => JsValue]("func", this.showStatus)
    ).addSubparser(
      Parser("done", "number of apps that finished analysis")
        .addDefault[ParsingResult => JsValue]("func", this.showDone)
    ).addSubparser(
      Parser("outstanding", "number of apps not yet analyzed")
        .addDefault[ParsingResult => JsValue]("func", this.showOutstanding)
    ).addSubparser(
      Parser("failed", "number of apps that failed analysis")
        .addFlag("group", "g", "group", "group apps by failure type")
        .addDefault[ParsingResult => JsValue]("func", this.showFailed)
    )

  private def showStatus(pargs: ParsingResult): JsValue = {
    showDone(pargs)
    showOutstanding(pargs)
    showFailed(pargs)
  }

  private def showDone(pargs: ParsingResult): JsValue = {
    val params = getParams(pargs)
    val experiment = Experiment(params.eid.toString)
    val analysis = InterfaceAnalysis.get(experiment)
    JsNull
  }

  private def getParams(pargs: ParsingResult): Params = {
    val eid = pargs.getValue[String]("id").toInt
    val apps = AppListParser.read(pargs.getValue[String]("app-list"))
    val listAll = pargs.getValue[Boolean]("listAll")
    Params(eid, apps, listAll)
  }

  private def showOutstanding(pargs: ParsingResult): JsValue = {
    val params = getParams(pargs)
    JsNull
  }

  private def showFailed(pargs: ParsingResult): JsValue = {
    val params = getParams(pargs)
    JsNull
  }

  private case class Params(eid: Int, list: MobileAppList, listAll: Boolean)

}
