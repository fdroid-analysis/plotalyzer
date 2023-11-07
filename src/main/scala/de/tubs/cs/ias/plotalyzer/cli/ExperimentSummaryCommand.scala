package de.tubs.cs.ias.plotalyzer.cli
import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.plotalyzer.analysis.{BasicExperimentSummary, ExperimentErrorSummary, FailedAnalysisSummary}
import de.tubs.cs.ias.plotalyzer.database.entities.Experiment
import de.tubs.cs.ias.plotalyzer.utility.Time
import spray.json.{JsNumber, JsObject, JsString, JsValue}

object ExperimentSummaryCommand extends Command {
  override val parser: Parser = Parser("summary", "summarize the experiment")
    .addSubparser(Parser("overview", "high level summary of the experiment")
                    .addDefault[ParsingResult => JsValue]("func", generateExperimentSummaryMain))
    .addSubparser(Parser("error", "summarize encountered errors")
                    .addDefault[ParsingResult => JsValue]("func", generateErrorSummaryMain))
    .addSubparser(Parser("failures", "summarize failed analysis and apps")
                    .addDefault[ParsingResult => JsValue]("func", generateFailureSummaryMain))

  private def generateExperimentSummaryMain(pargs: ParsingResult): JsValue = {
    val experimentId           = pargs.getValue[String]("id")
    val experiment: Experiment = Experiment(experimentId)
    JsObject("id" -> JsNumber(experiment.getId),
             "created" -> JsString(Time.format(experiment.getCreated)),
             "description" -> JsString(experiment.getDescription),
             "basicSummary" -> BasicExperimentSummary(experiment).toJson)
  }

  private def generateErrorSummaryMain(pargs: ParsingResult): JsValue = {
    val experimentId           = pargs.getValue[String]("id")
    val experiment: Experiment = Experiment(experimentId)
    JsObject("id" -> JsNumber(experiment.getId),
             "created" -> JsString(Time.format(experiment.getCreated)),
             "description" -> JsString(experiment.getDescription),
             "errorSummary" -> ExperimentErrorSummary(experiment).toJson)
  }

  private def generateFailureSummaryMain(pargs: ParsingResult): JsValue = {
    val experimentId           = pargs.getValue[String]("id")
    val experiment: Experiment = Experiment(experimentId)
    JsObject("id" -> JsNumber(experiment.getId),
             "created" -> JsString(Time.format(experiment.getCreated)),
             "description" -> JsString(experiment.getDescription),
             "failureSummary" -> FailedAnalysisSummary(experiment).toJson)
  }
}
