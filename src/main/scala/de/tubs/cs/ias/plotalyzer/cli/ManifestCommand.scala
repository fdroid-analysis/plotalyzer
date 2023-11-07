package de.tubs.cs.ias.plotalyzer.cli
import de.halcony.argparse.{OptionalValue, Parser, ParsingResult}
import de.tubs.cs.ias.plotalyzer.json.AppmiRankingListSet
import java.io.{File, FileWriter}
import scala.io.Source
import spray.json.{JsNumber, JsObject, JsString, JsValue, JsonParser}
import wvlet.log.LogSupport

object ManifestCommand extends Command with LogSupport {
  override val parser: Parser = Parser("manifest", "work with the manifest file")
    .addPositional("manifest", "the manifest.json")
    .addSubparser(
      Parser(
        "appMetaData",
        "using the manifest and ranking lists create a comprehensive meta data file"
      ).addPositional("rankingListFolder", "the folder with the ranking lists")
        .addDefault[ParsingResult => JsValue]("func", generateAppMetaDataMain)
    )
    .addSubparser(
      Parser(
        "generateOnlySet",
        "given an app metadata json, filter for apps matching specifications"
      ).addOptional(
          "categories",
          "c",
          "category",
          None,
          "app belongs to a certain category (csv list)"
        )
        .addOptional("bestCategory", "b", "best-category", None, "the apps best category")
        .addOptional(
          "rank",
          "r",
          "rank",
          None,
          "the apps rank either for the provided categories or if not provided best rank is used"
        )
        .addDefault[ParsingResult => JsValue]("func", printFilteredAppManifestMain)
    )

  private def generateAppMetaDataMain(pargs: ParsingResult): JsValue = {
    val manifest = pargs.getValue[String]("manifest")
    val listFolder = pargs.getValue[String]("rankingListFolder")
    JsObject(
      AppmiRankingListSet
        .getAppSummary(listFolder, manifest)
        .map(elem => elem.getId -> elem.toJson)
        .toMap
    )
  }

  private def printFilteredAppManifestMain(pargs: ParsingResult): JsValue = {
    val categories: Set[String] = pargs
      .getValueOrElse[String]("categories", "NOTHING")
      .split(",")
      .toSet
      .filterNot(_ == "NOTHING")
    val bestCategory: Option[String] = pargs.get[OptionalValue[String]]("bestCategory").value
    val rank: Option[Int] =
      pargs.get[OptionalValue[String]]("rank").value match {
        case Some(value) =>
          Some(value.toInt)
        case None =>
          None
      }
    val source = Source.fromFile(pargs.getValue[String]("manifest"))
    val json: JsObject =
      try {
        JsonParser(source.mkString).asJsObject
      } finally {
        source.close()
      }
    val out = new FileWriter(new File(pargs.getValue[String]("out")))
    try {
      json
        .fields
        .foreach { case (appId, appMetaData) =>
          try {
            val metaDataObj = appMetaData.asJsObject
            if (categories.isEmpty) {
              if (
                (bestCategory.isEmpty ||
                  bestCategory
                    .get == metaDataObj.fields("bestCategory").asInstanceOf[JsString].value) &&
                (rank.isEmpty ||
                  rank.get >= metaDataObj.fields("bestRank").asInstanceOf[JsNumber].value)
              ) {
                out.write(appId + "\n")
              }
            } else {
              if (bestCategory.nonEmpty)
                warn("categories is non empty, ignoring best category")
              if (
                metaDataObj
                  .fields("categories")
                  .asJsObject
                  .fields
                  .exists { case (category, catRank) =>
                    categories.contains(category) &&
                      (rank.isEmpty || rank.get >= catRank.asInstanceOf[JsNumber].value)
                  }
              )
                out.write(appId + "\n")
            }
          } catch {
            case err: Throwable =>
              error(s"$appId : ${err.getMessage}")
          }
        }
    } finally {
      out.flush()
      out.close()
    }
    null
  }
}
