package de.tubs.cs.ias.plotalyzer.cli
import com.github.vickumar1981.stringdistance.StringDistance._
import de.halcony.argparse.{OptionalValue, Parser, ParsingResult}
import de.tubs.cs.ias.plotalyzer.Plotalyzer.getOnlyApps
import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter.HostPathFilterTypes.{EXODUS, GENERIC}
import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter.TrafficFilter
import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.{RequestTrackingEndpointAnalysis, TrafficCollectionAnalysisConfig, TrafficSummary}
import de.tubs.cs.ias.plotalyzer.database.entities.adblock.RequestRecipient
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.{Request, TrafficCollection}
import de.tubs.cs.ias.plotalyzer.database.entities.{Experiment, InterfaceAnalysis}
import de.tubs.cs.ias.plotalyzer.json.AppRequestComparisonJsonProtocol.AppRequestComparisonJsonFormat
import de.tubs.cs.ias.plotalyzer.json.{AppRequestComparison, RequestEquivalence}
import de.tubs.cs.ias.plotalyzer.utility.{AsciiProgressBar, Time}
import scala.collection.immutable.SortedMap
import spray.json.{JsArray, JsNumber, JsObject, JsString, JsValue, enrichAny}

object TrafficCommand extends Command {
  override val parser: Parser = Parser("traffic", "analyze the collected traffic data")
    .addSubparser(
      Parser("summary", "list all the requests collected during the experiment")
        .addOptional(
          "timespan",
          "s",
          "timespan-seconds",
          None,
          "how many seconds of the experiment since start are part of the summary"
        )
        .addOptional(
          "filterJson",
          "j",
          "filter-file",
          None,
          "path to an exodus tracker json for filtering"
        )
        .addOptional(
          "filterType",
          "t",
          "filter-type",
          None,
          "what type of request filter {Exodus,Generic}"
        )
        .addOptional("only", "o", "only", None, "only analyze the provided apps")
        .addDefault[ParsingResult => JsValue]("func", generateTrafficSummary)
    )
    .addSubparser(
      Parser("analyzeCollection", "analyze the collected traffic")
        .addSubparser(
          Parser("parseEndpoints", "use endpoint parsers to extract PII from traffic")
            .addOptional(
              "trafficConfig",
              "c",
              "config",
              Some("./resources/trafficCollection/config.json")
            )
            .addOptional("only", "o", "only", None, "only analyze the provided apps")
            .addDefault[ParsingResult => JsValue]("func", generateTrafficCollectionAnalysisMain)
        )
        .addSubparser(
          Parser("llm", "use LLM to try to extract PII from traffic")
            .addOptional("llmConfig", "c", "config", Some("./resources/trafficCollection/llm.conf"))
            .addOptional("only", "o", "only", None, "only analyze appIds contained in a file")
            .addDefault[ParsingResult => JsValue]("func", generateTrafficCollectionAnalysisLlmMain)
        )
    )
    .addSubparser(
      Parser("compareAppRequests", "compare requests between experiments for apps that are in both")
        .addPositional("otherExperiment", "id of the other experiment")
        .addDefault[ParsingResult => JsValue]("func", this.compareAppRequests)
    )
    .addSubparser(
      Parser(
        "estimateRecipientType",
        "heuristically determine the recipient of a request {developer, thirdparty}"
      ).addOptional(
          "similarity",
          "s",
          description =
            "similarity between host and app_id to consider them associated (default: 0.8)",
          default = Some("0.8")
        )
        .addDefault[ParsingResult => JsValue]("func", this.estimateRecipientType)
    )

  private def generateTrafficSummary(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id")
    val experiment: Experiment = Experiment(experimentId)
    val timespan: Option[TrafficFilter] =
      pargs.get[OptionalValue[String]]("timespan").value match {
        case Some(value) =>
          Some(TrafficFilter.getTimeFilter(value.toLong))
        case None =>
          None
      }
    val hostName: Option[TrafficFilter] =
      (
        pargs.get[OptionalValue[String]]("filterJson").value,
        pargs.get[OptionalValue[String]]("filterType").value
      ) match {
        case (Some(json), Some(filterType)) =>
          filterType match {
            case "Exodus" =>
              Some(TrafficFilter.getHostPathFilter(json, EXODUS))
            case "Generic" =>
              Some(TrafficFilter.getHostPathFilter(json, GENERIC))
            case x =>
              throw new RuntimeException(s"the filter type $x is unknown")
          }
        case (None, None) =>
          None
        case _ =>
          throw new RuntimeException(
            "-j/--filter-file and -t/--filter-type have to be used together"
          )
      }
    val only = getOnlyApps(pargs)
    TrafficSummary(experiment, List(timespan, hostName).filter(_.nonEmpty).map(_.get), only).toJson
  }

  private def generateTrafficCollectionAnalysisMain(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id")
    val only = getOnlyApps(pargs)
    val confPath = pargs.getValue[String]("trafficConfig")
    val experiment: Experiment = Experiment(experimentId)
    JsObject(
      "id" -> JsNumber(experiment.getId),
      "created" -> JsString(Time.format(experiment.getCreated)),
      "description" -> JsString(experiment.getDescription),
      "trafficTrackingEndpoints" ->
        RequestTrackingEndpointAnalysis(
          experiment,
          TrafficCollectionAnalysisConfig.get(confPath),
          only
        ).toJson
    )
  }

  private def compareAppRequests(pargs: ParsingResult): JsValue = {
    val thisExpId = pargs.getValue[String]("id").toInt
    val thatExpId = pargs.getValue[String]("otherExperiment").toInt
    val thisAnalyses =
      Experiment(thisExpId.toString).getLatestSuccessfulAppInterfaceAnalysesWithTraffic
    val thatAnalyses =
      Experiment(thatExpId.toString).getLatestSuccessfulAppInterfaceAnalysesWithTraffic
    val comparisons = compareExperiments(thisAnalyses, thatAnalyses)
    val same = comparisons.filter(_.jaccardIndex >= 1.0d)
    val different = comparisons.filter(_.jaccardIndex < 1.0d).sortBy(_.jaccardIndex).reverse

    JsObject(
      "thisExperimentId" -> JsNumber(thisExpId),
      "thatExperimentId" -> JsNumber(thatExpId),
      "same" ->
        JsObject(
          "count" -> JsNumber(same.size),
          "apps" -> JsArray(same.map(_.toJson).toVector),
          "requestFilterMatchCount" -> JsObject(
            "both" -> groupByTrackingCount(same, _.requestsBothTrackingCount),
            "thisOnly" -> JsNumber(0),
            "thatOnly" -> JsNumber(0)
          )
       ),
      "different" ->
        JsObject(
          "count" -> JsNumber(different.size),
          "requestFilterMatchCount" -> JsObject(
            "both" ->  groupByTrackingCount(different, _.requestsBothTrackingCount),
            "thisOnly" ->  groupByTrackingCount(different, _.requestsThisOnlyTrackingCount),
            "thatOnly" ->  groupByTrackingCount(different, _.requestsThatOnlyTrackingCount)
          ),
          "apps" -> JsArray(different.map(_.toJson).toVector)
        )
    )
  }

  private def groupByTrackingCount(
    appComparisons: Seq[AppRequestComparison],
    fieldSelector: AppRequestComparison => Int
  ): JsObject  = {
    val groupByTrackingCount = appComparisons
      .groupBy(fieldSelector)
      .map(trackingCount_Apps => trackingCount_Apps._1 -> JsNumber(trackingCount_Apps._2.size))
    val sorted = SortedMap.from(groupByTrackingCount)
    val sortedJson = sorted.map(entry => entry._1.toString -> entry._2)
    JsObject(sortedJson)
  }

  private def compareExperiments(
      thisAnalyses: Seq[InterfaceAnalysis],
      thatAnalyses: Seq[InterfaceAnalysis]
  ): Seq[AppRequestComparison] = {
    val bar = AsciiProgressBar.create("Comparing app requests", thisAnalyses.size.toLong)
    try {
      thisAnalyses
        .map { thisAnalysis =>
          try {
            val appId = thisAnalysis.getApp.id
            val thatAnalysis = thatAnalyses.find(analysis => appId == analysis.getApp.id)
            thatAnalysis match {
              case Some(thatAnalysis) =>
                Some(compareRequests(thisAnalysis, thatAnalysis))
              case None =>
                None
            }
          } finally {
            bar.step()
          }
        }
        .filter(_.nonEmpty)
        .map(_.get)
    } finally {
      bar.close()
    }
  }

  private def compareRequests(
      thisAnalysis: InterfaceAnalysis,
      thatAnalysis: InterfaceAnalysis
  ): AppRequestComparison = {
    assert(thisAnalysis.getApp.id == thatAnalysis.getApp.id)
    val simpleCollection =
      (analysis: InterfaceAnalysis) =>
        analysis
          .getTrafficCollections
          .filter(_.getComment == "Simple Traffic Collection")
          .sortBy(_.getStart)
          .reverse
    val collectionToRequests =
      (collection: TrafficCollection) => collection.getRequests.filter(_.error.isEmpty)
    val thisCollections = simpleCollection(thisAnalysis)
    val thatCollections = simpleCollection(thatAnalysis)
    assert(thisCollections.size == 1 && thatCollections.size == 1)
    val thisCollection = thisCollections.head
    val thatCollection = thatCollections.head
    val thisRequests = collectionToRequests(thisCollection)
    val thatRequests = collectionToRequests(thatCollection)

    val toUrlReqs =
      (reqs: List[Request]) =>
        reqs.map(req => req -> req.getUrl).groupBy(_._2).view.mapValues(_.map(_._1)).toMap
    val thisUrlReqs = toUrlReqs(thisRequests)
    val thatUrlReqs = toUrlReqs(thatRequests)
    val intersection = thisUrlReqs.keySet.intersect(thatUrlReqs.keySet)
    val thisOnly = thisUrlReqs.keySet.removedAll(intersection)
    val thatOnly = thatUrlReqs.keySet.removedAll(intersection)

    val urlToReqs: String => RequestEquivalence =
      (url: String) => {
        val requests =
          thisUrlReqs.getOrElse(url, List.empty) ++ thatUrlReqs.getOrElse(url, List.empty)
        RequestEquivalence(url, requests.map(_.id))
      }
    new AppRequestComparison(
      thisAnalysis.getApp.id,
      thisCollection.getId,
      thatCollection.getId,
      intersection.map(urlToReqs).toList,
      thisOnly.map(urlToReqs).toList,
      thatOnly.map(urlToReqs).toList
    )
  }

  private def estimateRecipientType(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id").toInt
    val similarity = pargs.getValue[String]("similarity").toFloat
    val experiment = Experiment.apply(experimentId.toString)
    val recipientsRequestIds = RequestRecipient.getAll.map(_.requestId)
    val analyses = experiment.getLatestSuccessfulAppInterfaceAnalysesWithTraffic
    analyses.foreach { analysis =>
      val appId = analysis.getApp.id.split('.').drop(1).mkString(".")
      val requests = analysis.getTrafficCollections.flatMap(_.getRequests).filter(_.error.isEmpty)
      requests.foreach { request =>
        val hostReversed = reverseHost(request.host)
        val overlap = JaroWinkler.score(appId, hostReversed)
        if (recipientsRequestIds.contains(request.id)) {
          RequestRecipient.update(RequestRecipient(request.id, overlap > similarity))
        } else {
          RequestRecipient.insert(RequestRecipient(request.id, overlap > similarity))
        }
      }
    }
    JsString("yo")
  }

  private def reverseHost(host: String): String = {
    val split = host.split('.')
    val reverseSplit = split.reverse.drop(1)
    val reverseHost = reverseSplit.mkString(".")
    reverseHost
  }

  private def generateTrafficCollectionAnalysisLlmMain(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id")
    val onlyFile = pargs.get[OptionalValue[String]]("only")
    // call python
    JsObject("ey" -> JsString("yoyo"))
  }
}
