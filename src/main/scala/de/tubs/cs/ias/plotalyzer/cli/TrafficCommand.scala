package de.tubs.cs.ias.plotalyzer.cli
import com.github.vickumar1981.stringdistance.StringDistance._
import de.halcony.argparse.{OptionalValue, Parser, ParsingResult}
import de.tubs.cs.ias.plotalyzer.Plotalyzer.getOnlyApps
import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.{
  RequestTrackingEndpointAnalysis,
  TrafficCollectionAnalysisConfig,
  TrafficSummary
}
import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter.HostPathFilterTypes.{
  EXODUS,
  GENERIC
}
import de.tubs.cs.ias.plotalyzer.analysis.trafficanalysis.filter.TrafficFilter
import de.tubs.cs.ias.plotalyzer.database.entities.adblock.RequestRecipient
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.{Request, TrafficCollection}
import de.tubs.cs.ias.plotalyzer.database.entities.{Experiment, InterfaceAnalysis}
import de.tubs.cs.ias.plotalyzer.json.AppRequestComparisonJsonProtocol.AppRequestComparisonJsonFormat
import de.tubs.cs.ias.plotalyzer.json.{AppRequestComparison, RequestEquivalence}
import de.tubs.cs.ias.plotalyzer.utility.{AsciiProgressBar, Time}
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
          description = "similarity between host and app_id to consider them associated (default: 0.8)",
          default = Some("0.8"),
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
    // get latest successful analysis per app for each exp
    // map appId to requests
    // for app in this: get app in that
    // diff requests
    // insert into results object
    // differentiate results between: requests -same, -different
    // when different requests: lists req in both, req only in this, req only in that
    // request is different, when contacting different scheme, host and path up until query params
    val thisAnalyses = Experiment(thisExpId.toString).getLatestSuccessfulInterfaceAnalyses
    val thatAnalyses = Experiment(thatExpId.toString).getLatestSuccessfulInterfaceAnalyses
    val bar = AsciiProgressBar.create("Comparing app requests", thisAnalyses.size.toLong)
    val comparisons = thisAnalyses
      .map { thisAnalysis =>
        val appId = thisAnalysis.getApp.id
        val thatAnalysis = thatAnalyses.find(analysis => appId == analysis.getApp.id)
        bar.step()
        thatAnalysis match {
          case Some(thatAnalysis) =>
            Some(compareRequests(thisAnalysis, thatAnalysis))
          case None =>
            None
        }
      }
      .filter(_.nonEmpty)
      .map(_.get)
    bar.close()
    val same = comparisons.filter(_.jaccardIndex >= 1.0d)
    val different = comparisons.filter(_.jaccardIndex < 1.0d).sortBy(1 - _.jaccardIndex)
    JsObject(
      "thisExperimentId" -> JsNumber(thisExpId),
      "thatExperimentId" -> JsNumber(thatExpId),
      "same" ->
        JsObject("count" -> JsNumber(same.size), "apps" -> JsArray(same.map(_.toJson).toVector)),
      "different" ->
        JsObject(
          "count" -> JsNumber(different.size),
          "apps" -> JsArray(different.map(_.toJson).toVector)
        )
    )
  }

  private def compareRequests(
      thisAnalysis: InterfaceAnalysis,
      thatAnalysis: InterfaceAnalysis
  ): AppRequestComparison = {
    assert(thisAnalysis.getApp.id == thatAnalysis.getApp.id)
    val simpleCollection =
      (analysis: InterfaceAnalysis) =>
        analysis.getTrafficCollections.filter(_.getComment == "Simple Traffic Collection")
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
    val jaccardIndex =
      intersection.size.toDouble / (thisOnly.size + intersection.size + thatOnly.size)

    val urlToReqs: String => RequestEquivalence =
      (url: String) => {
        val requests =
          thisUrlReqs.getOrElse(url, List.empty) ++ thatUrlReqs.getOrElse(url, List.empty)
        RequestEquivalence(url, requests.map(_.id))
      }
    AppRequestComparison(
      thisAnalysis.getApp.id,
      thisCollection.getId,
      thatCollection.getId,
      intersection.map(urlToReqs).toList,
      thisOnly.map(urlToReqs).toList,
      thatOnly.map(urlToReqs).toList,
      jaccardIndex
    )
  }

  private def estimateRecipientType(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id").toInt
    val similarity = pargs.getValue[String]("similarity").toFloat
    val experiment = Experiment.apply(experimentId.toString)
    val recipientsRequestIds = RequestRecipient.getAll.map(_.requestId)
    val analysis = experiment.getLatestSuccessfulInterfaceAnalyses
    analysis.foreach { analysis =>
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
}
