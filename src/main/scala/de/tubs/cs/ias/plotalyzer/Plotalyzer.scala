package de.tubs.cs.ias.plotalyzer

import de.halcony.argparse.{OptionalValue, Parser, ParsingException, ParsingResult}
import de.tubs.cs.ias.plotalyzer.analysis.consentdialog.{ConsentDialogAnalysis, SpotcheckGenerator}
import de.tubs.cs.ias.plotalyzer.analysis.tcfdetection.{DynamicTCFDetection, StaticTCFDetection, TCFDetectionConf}
import de.tubs.cs.ias.plotalyzer.cli._
import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.{Experiment, InterfaceAnalysis}
import de.tubs.cs.ias.plotalyzer.utility.Time
import java.io.{BufferedWriter, File, FileWriter}
import scala.io.Source
import spray.json.{JsArray, JsNumber, JsObject, JsString, JsValue}
import wvlet.log.LogSupport

object Plotalyzer extends LogSupport {

  val parser: Parser = Parser("plotalyzer", "takes a scala-appanalyzer database and generates json for further processing")
    .addOptional("database-conf", "db", "database", Some("./db.json"), "configuration file for the database connection")
    .addPositional("id", "the id of the experiment to analyze")
    .addPositional("out", "the path to print the result into (folder for screenshot json for data)")
    .addSubparser(ManifestCommand.parser)
    .addSubparser(Parser("tcfDetection", "analyze a set of apps for any tcf indicating values")
        .addOptional("only", "o", "only", None, "only analyze the provided apps")
        .addOptional("tcfConfig", "c", "config", Some("./resources/tcfDetection/TCFConfig.json"))
        .addSubparser(Parser("dynamic", "perform analysis on dynamically collected data")
            .addDefault[ParsingResult => JsValue]("func", generateDynamicTcfSummaryMain))
        .addSubparser(Parser("static", "use statically extracted file data")
            .addPositional("os", "the operating system for which the binary initially was {Android,iOS}")
            .addPositional("json", "the json containing the extracted files per app")
            .addDefault[ParsingResult => JsValue]("func", generateStaticTcfSummaryMain)))
    .addSubparser(Parser("screenshot", "dump all screenshots associated with an app")
        .addPositional("app", "the app id")
        .addDefault[ParsingResult => JsValue]("func", dumpScreenshotsMain))
    .addSubparser(ExperimentSummaryCommand.parser)
    .addSubparser(TrafficCommand.parser)
    .addSubparser(Parser("consentDialog", "analyze a consent dialog analysis experiment")
        .addOptional("analysisConfig", "c", "config", Some("./resources/consentDialog/config.json"), "the config for the consent dialog analysis")
        .addOptional("only", "o", "only", None, "only analyze the provided apps")
        .addSubparser(Parser("analyze", "perform an analysis")
            .addDefault[ParsingResult => JsValue]("func", generateConsentDialogAnalysisMain))
        .addSubparser(Parser("spotcheck", "perform an spotcheck of the analysis results")
            .addOptional("include", "i", "include", None, "path to a file containing line by line list of to be included apps")
            .addOptional("fill", "f", "fill", None, "amount of apps that should be spot checked")
            .addDefault[ParsingResult => JsValue]("func", generateConsentDialogAnalysisSpotcheckMain)))
    .addSubparser(ExperimentStatusCommand.parser)
    .addSubparser(PrivacyLabelCommand.parser)
    .addSubparser(StaticAnalysisCommand.parser)

  def main(args: Array[String]): Unit = {
    try {
      val pargs = parser.parse(args)
      Database.initialize(pargs)
      val ret: Option[JsValue] = Option(pargs.getValue[ParsingResult => JsValue]("func")(pargs))
      ret match {
        case Some(value) =>
          val out = new BufferedWriter(new FileWriter(new File(pargs.getValue[String]("out"))))
          try {
            out.write(value.prettyPrint)
          } finally {
            out.flush()
            out.close()
          }
        case None =>
      }
    } catch {
      case x: ParsingException =>
        println(x.getMessage())
    }
  }

  def getOnlyApps(pargs: ParsingResult): Set[String] = {
    pargs.get[OptionalValue[String]]("only").value match {
      case Some(value) =>
        if (new File(value).exists()) {
          val source = Source.fromFile(value)
          try {
            source.getLines().map(_.trim).toSet
          } finally {
            source.close()
          }
        } else {
          value.split(',').toSet
        }
      case None =>
        Set()
    }
  }

  private def generateConsentDialogAnalysisMain(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id")
    val config = pargs.getValue[String]("analysisConfig")
    val only = getOnlyApps(pargs)
    val experiment: Experiment = Experiment(experimentId)
    val res = ConsentDialogAnalysis(experiment, config, only)
    JsObject(
      "_failed" -> JsArray(res.getFailedAnalysisIds.map(elem => JsNumber(elem)).toVector),
      "id" -> JsNumber(experiment.getId),
      "created" -> JsString(Time.format(experiment.getCreated)),
      "description" -> JsString(experiment.getDescription),
      "consentDialogAnalysis" -> res.toJson
    )
  }

  private def generateConsentDialogAnalysisSpotcheckMain(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id")
    val config = pargs.getValue[String]("analysisConfig")
    val only = getOnlyApps(pargs)
    val include =
      pargs.get[OptionalValue[String]]("include").value match {
        case Some(value) =>
          val source = Source.fromFile(value)
          try {
            source.getLines().toList
          } finally {
            source.close()
          }
        case None =>
          List()
      }
    val experiment: Experiment = Experiment(experimentId)
    val res = ConsentDialogAnalysis(experiment, config, only)
    val out = pargs.getValue[String]("out")
    val fill =
      pargs.get[OptionalValue[String]]("fill").value match {
        case Some(value) =>
          Some(value.toInt)
        case None =>
          None
      }
    info(s"generating spotcheck for $fill apps including ${include.length} predefined")
    SpotcheckGenerator.generate(res, include, fill, out)
    null
  }

  private def generateStaticTcfSummaryMain(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id")
    val filePath = pargs.getValue[String]("json")
    val only = getOnlyApps(pargs)
    val conf = TCFDetectionConf.apply(pargs.getValue[String]("tcfConfig"))
    val os = pargs.getValue[String]("os")
    JsObject(
      "id" -> JsNumber(experimentId),
      "type" -> JsString("static"),
      "summary" -> new StaticTCFDetection(filePath, conf, os, only).toJson
    )
  }

  private def generateDynamicTcfSummaryMain(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id")
    val experiment: Experiment = Experiment(experimentId)
    val only = getOnlyApps(pargs)
    val config = pargs.getValue[String]("tcfConfig")
    JsObject(
      "id" -> JsNumber(experimentId),
      "type" -> JsString("dynamic"),
      "summary" -> DynamicTCFDetection(experiment, TCFDetectionConf.apply(config), only).toJson
    )
  }

  private def dumpScreenshotsMain(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id")
    val app = pargs.getValue[String]("app")
    val out = pargs.getValue[String]("out")
    assert(!new File(out).exists())
    InterfaceAnalysis
      .get(Experiment(experimentId), app)
      .foreach { analysis =>
        analysis.dumpScreenshot(out)
      }
    null
  }

}
