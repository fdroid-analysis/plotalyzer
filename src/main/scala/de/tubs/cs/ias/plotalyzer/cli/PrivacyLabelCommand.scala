package de.tubs.cs.ias.plotalyzer.cli
import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.plotalyzer.analysis.privacylabel.{FDroidPrivacyLabel, PrivacyLabelAnalysis}
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
          val listToLabels = Map(
            FilterListIds.easylist -> List("Ads"),
            FilterListIds.easyprivacy -> List("Tracking"),
            FilterListIds.easylist_noelemhide -> List("Ads"),
            FilterListIds.easylistgermany -> List("Ads"),
            FilterListIds.peterlowe -> List("Tracking", "Ads")
          )
          PrivacyLabelAnalysis.privacyLabelConsistency(
            experimentId,
            labelFolder,
            ".json",
            (appId, fileNames) => fileNames.find(_.startsWith(appId)),
            FDroidPrivacyLabel.read,
            listToLabels.keySet.toList
          )
        case _ => JsString("labelFormat not implemented") // implement me

      }
    outputJson
  }

  private object FilterListIds {
    val easylist = 1
    val easyprivacy = 2
    val easylistgermany = 4
    val easylist_noelemhide = 5
    val peterlowe = 6
  }
}
