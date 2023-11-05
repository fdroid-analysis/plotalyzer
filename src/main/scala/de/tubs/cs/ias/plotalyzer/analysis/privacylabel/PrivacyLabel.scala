package de.tubs.cs.ias.plotalyzer.analysis.privacylabel

import de.tubs.cs.ias.applist.fdroid.FDroidJsonProtocol.PrivacyLabelFormat
import de.tubs.cs.ias.plotalyzer.database.entities.adblock.{FilterList, RequestMatch}
import de.tubs.cs.ias.util.{FileSystemInteraction => fsi}
import spray.json.{JsValue, JsonParser, enrichAny}

sealed trait PrivacyLabel {
  val listIdToLabel: Map[Int, List[String]]

  def toJson: JsValue

  def isConsistent(requestMatch: RequestMatch): Boolean

  def isConsistent(filterList: FilterList): Boolean
}

case class FDroidPrivacyLabel(antiFeatures: List[String]) extends PrivacyLabel {

  override val listIdToLabel: Map[Int, List[String]] = FDroidPrivacyLabel.listIdToLabel
  override def isConsistent(requestMatch: RequestMatch): Boolean = {
    assert(listIdToLabel.keySet.contains(requestMatch.listId))
    assert(requestMatch.isMatch)
    if (this.antiFeatures.intersect(listIdToLabel(requestMatch.listId)).isEmpty) {
      // antiFeatures and labels don't intersect --> missing label
      false
    } else {
      // antiFeatures and labels intersect --> app labeled correctly
      true
    }
  }

  override def toJson: JsValue = de.tubs.cs.ias.applist.fdroid.PrivacyLabel(antiFeatures).toJson

  override def isConsistent(filterList: FilterList): Boolean = {
    assert(listIdToLabel.keySet.contains(filterList.id))
    if (antiFeatures.intersect(listIdToLabel(filterList.id)).nonEmpty) {
      true
    } else {
      false
    }
  }
}

object FDroidPrivacyLabel {

  val listIdToLabel: Map[Int, List[String]] = Map(
    FilterListIds.easylist -> List("Ads"),
    FilterListIds.easyprivacy -> List("Tracking"),
    FilterListIds.easylist_noelemhide -> List("Ads"),
    FilterListIds.easylistgermany -> List("Ads"),
    FilterListIds.peterlowe -> List("Tracking", "Ads")
  )

  def read(filePath: String): Option[FDroidPrivacyLabel] = {
    val rawFileContent = fsi.readInTextFile(filePath)
    val privacyLabel = JsonParser(rawFileContent)
      .convertTo[de.tubs.cs.ias.applist.fdroid.PrivacyLabel]
    val fdroidLabel = FDroidPrivacyLabel(privacyLabel.antiFeatures)

    Some(fdroidLabel)
  }
}

private object FilterListIds {
  val easylist = 1
  val easyprivacy = 2
  val easylistgermany = 4
  val easylist_noelemhide = 5
  val peterlowe = 6
}
