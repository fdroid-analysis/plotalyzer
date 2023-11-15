package de.tubs.cs.ias.plotalyzer.cli
import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.plotalyzer.database.entities.exodus.{AppLibrary, AppTracker, Tracker}
import de.tubs.cs.ias.plotalyzer.json.{ExodusOutput, ExodusTrackerIndex}
import de.tubs.cs.ias.plotalyzer.json.ExodusOutputJsonProtocol.exodusOutputFormat
import de.tubs.cs.ias.plotalyzer.json.ExodusTrackerIndexJsonProtocol.exodusTrackerIndexFormat
import de.tubs.cs.ias.plotalyzer.utility.AsciiProgressBar
import de.tubs.cs.ias.util.{FileSystemInteraction => fsi}
import java.io.File
import scala.collection.mutable.HashMap
import scala.sys.process.{Process, ProcessLogger}
import spray.json.{JsArray, JsNumber, JsObject, JsString, JsValue, JsonParser, enrichAny}

object StaticAnalysisCommand extends Command {
  override val parser: Parser = Parser("staticAnalysis", "use static analysis").addSubparser(
    Parser("exodus", "use exodus to scan apks for libraries and trackers")
      .addSubparser(
        Parser("scan", "scan apks in a folder")
          .addPositional("folder", "apk folder")
          .addDefault[ParsingResult => JsValue]("func", this.exodusScanApks)
      )
      .addSubparser(
        Parser("evaluate", "evaluate scan results")
          .addPositional("exodusFile", "an exodus_tracker json file to use")
          .addDefault[ParsingResult => JsValue]("func", this.exodusEvaluateExperiment)
      )
  )

  private def exodusScanApks(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id").toInt
    val folderPath = pargs.getValue[String]("folder")
    val apks = fsi.getFiles(folderPath, Some(".apk"))
    val bar = AsciiProgressBar.create("scanning apks", apks.size.toLong)
    val exodusOutput = apks.map { apkFile =>
      bar.setExtraMessage(apkFile)
      bar.step()
      val cmdOut = new StringBuilder()
      val cmdError = new StringBuilder()
      val cmd =
        s"docker run -v $folderPath:/app --rm -i exodusprivacy/exodus-standalone -j -e 0 /app/$apkFile"
      val ret = Process(cmd, cwd = new File(folderPath))
        .run(ProcessLogger(fOut => cmdOut.append(fOut), fErr => cmdError.append(fErr)))
      val exodusOutput =
        ret.exitValue() match {
          case 0 =>
            Some(JsonParser(cmdOut.toString()).convertTo[ExodusOutput])
          case _ =>
            None
        }
      exodusOutput
    }
    // insert values
    val successful = exodusOutput.filter(_.isDefined).map(_.get)
    // insert new trackers
    val allTrackers = Tracker.getAll
    val newTrackers =
      successful.flatMap(_.trackers).map(Tracker.apply).filterNot(allTrackers.contains).toSet
    newTrackers.foreach(Tracker.insert)

    // parse libs and trackers
    val appLibs = AppLibrary.getAll
    val appTrackers = AppTracker.getAll
    val libsTracker = successful.map { exodusOutput =>
      val appId = exodusOutput.application.handle
      val versionCode = exodusOutput.application.version_code.toInt
      val libs = exodusOutput
        .application
        .libraries
        .map(lib => AppLibrary(appId, versionCode, lib, experimentId))
        .filterNot(appLibs.contains)
      val trackers = exodusOutput
        .trackers
        .map(tracker => AppTracker(appId, versionCode, tracker.id, experimentId))
        .filterNot(appTrackers.contains)
      (libs, trackers)
    }
    libsTracker.foreach { libsTracker =>
      libsTracker._1.foreach(AppLibrary.insert)
      libsTracker._2.foreach(AppTracker.insert)
    }

    // map to json output
    val successfulJson = successful.map(_.toJson).toVector
    val apps = JsArray(successfulJson)
    JsObject("apps" -> apps)
  }

  private def exodusEvaluateExperiment(pargs: ParsingResult): JsValue = {
    val experimentId = pargs.getValue[String]("id").toInt
    val exodusFilePath = pargs.getValue[String]("exodusFile")
    val exodusIndex = JsonParser(fsi.readInTextFile(exodusFilePath)).convertTo[ExodusTrackerIndex]
    val allTrackers = Tracker.getAll
    val allTrackerNames = allTrackers.map(tracker => tracker.id -> tracker.name).toMap
    val indexTrackers = {
      allTrackers
        .map { tracker =>
          tracker.id -> exodusIndex.trackers.find(_.name == tracker.name)
        }
        .toMap
    }
    val trackerCategories =
      indexTrackers.filter(_._2.isDefined).view.mapValues(_.get).mapValues(_.category).toMap

    val appTrackers = AppTracker.getByExperiment(experimentId).groupBy(_.appId)
    // map to; category -> List[(AppId, AppVersion, List[TrackerName]))
    val categoryApps: HashMap[String, List[(String, Int, List[String])]] = HashMap()
    appTrackers.foreach { case (appId, trackers) =>
      val versionCode = trackers.head.versionCode
      val categories =
        trackers
          .map(tracker => tracker -> trackerCategories.get(tracker.trackerId))
          .filter(_._2.isDefined)
          .map(tracker => tracker._1 -> tracker._2.get)
          .flatMap { tracker =>
            if (tracker._2.nonEmpty) {
              tracker._2.map(category => tracker._1 -> category)
            } else { // keep libraries with no assigned categories
              List(tracker._1 -> "")
            }
          }
          .groupBy(_._2)
          .view
          .mapValues(_.map(_._1))
          .toMap
      categories
        .keys
        .foreach { category =>
          val trackers = categories(category)
          val trackerNames = trackers.map(_.trackerId).map(allTrackerNames.get).map(_.get)
          val appVersionTrackers = (appId, versionCode, trackerNames)

          val apps = categoryApps.getOrElse(category, List())
          val newApps = apps ++ List(appVersionTrackers)
          categoryApps.put(category, newApps)
        }
    }

    var outputJson: Map[String, JsValue] =
      categoryApps
        .view
        .mapValues { apps =>
          apps.map { appVersionTrackers =>
            JsObject(
              "appId" -> JsString(appVersionTrackers._1),
              "versionCode" -> JsNumber(appVersionTrackers._2),
              "trackers" -> JsArray(appVersionTrackers._3.map(JsString.apply).toVector)
            )
          }
        }
        .mapValues(_.toVector)
        .mapValues(apps => JsArray(apps))
        .toMap
    outputJson = outputJson ++ Map("experimentId" -> JsNumber(experimentId))

    JsObject(outputJson)
  }
}
