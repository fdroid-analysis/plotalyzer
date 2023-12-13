package de.tubs.cs.ias.plotalyzer.cli
import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.plotalyzer.analysis.privacylabel.{FDroidPrivacyLabel, PrivacyLabelAnalysis}
import de.tubs.cs.ias.plotalyzer.database.entities.adblock.FilterList
import de.tubs.cs.ias.plotalyzer.database.entities.adblock.FilterList.Lists.BlockList
import spray.json.{JsString, JsValue}

object PrivacyLabelCommand extends Command {
  override val parser: Parser = Parser("privacyLabel", "evaluate privacy labels").addSubparser(
    Parser("consistency", "check consistency of label and recorded requests")
      .addPositional("labelFolder", "folder where the privacy labels are stored")
      .addPositional("labelFormat", "format of the labels [fdroid,googleplay,appstore]")
      .addDefault[ParsingResult => JsValue]("func", this.checkLabelConsistency)
  )

  private def checkLabelConsistency(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id").toInt
    val labelFolder = pargs.getValue[String]("labelFolder")
    val labelFormat = pargs.getValue[String]("labelFormat")
    val outputJson =
      labelFormat match {
        case "fdroid" =>
          val listToLabels: Map[BlockList, List[String]] = Map(
            FilterList.Lists.easylist -> List("Ads"),
            FilterList.Lists.easyprivacy-> List("Tracking"),
            FilterList.Lists.easylist_noelemhide -> List("Ads"),
            FilterList.Lists.easylistgermany -> List("Ads"),
            FilterList.Lists.peterlowe -> List("Tracking", "Ads")
          )
          PrivacyLabelAnalysis.privacyLabelConsistency(
            experimentId,
            labelFolder,
            ".json",
            (appId, fileNames) => fileNames.find(_.startsWith(appId)),
            FDroidPrivacyLabel.read,
            listToLabels.keySet.map(_.id).toList
          )
        case _ => JsString("labelFormat not implemented") // implement me

      }
    outputJson
  }
}
