package de.tubs.cs.ias.plotalyzer.cli
import de.halcony.argparse.{Parser, ParsingResult}
import de.tubs.cs.ias.plotalyzer.database.entities.exodus.{AppLibrary, AppTracker, Tracker}
import de.tubs.cs.ias.plotalyzer.json.ExodusOutput
import de.tubs.cs.ias.plotalyzer.json.ExodusOutputJsonProtocol.exodusOutputFormat
import de.tubs.cs.ias.plotalyzer.utility.AsciiProgressBar
import de.tubs.cs.ias.util.{FileSystemInteraction => fsi}
import java.io.File
import scala.sys.process.{Process, ProcessLogger}
import spray.json.{JsArray, JsObject, JsValue, JsonParser, enrichAny}

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
    JsObject()
  }
}
