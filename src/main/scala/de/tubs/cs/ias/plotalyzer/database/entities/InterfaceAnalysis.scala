package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection.{
  TrafficCollection,
  TrafficCollectionCache
}
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import spray.json.JsValue
import wvlet.log.LogSupport

import java.io.File
import java.time.ZonedDateTime

class InterfaceAnalysis(
    id: Int,
    experiment: Int,
    app: MobileApp,
    description: String,
    start: ZonedDateTime,
    end: ZonedDateTime,
    success: Boolean
)(implicit database: Database)
    extends LogSupport {

  private var appPreferences: Option[List[AppPreferences]] = None
  private var trafficCollections: Option[List[TrafficCollection]] = None
  private var interfaceChain: Option[InterfaceInteractionChain] = None
  private var interfaceErrors: Option[List[InterfaceAnalysisError]] = None
  private var experimentErrors: Option[List[ExperimentError]] = None

  def dumpScreenshot(baseFolder: String): Unit = {
    val folder = new File(baseFolder + "/" + getDescription.replace(' ', '_'))
    assert(!folder.exists())
    folder.mkdirs()
    getInterfaceChain.dumpScreenshots(folder.getPath + "/")
  }

  def getDescription: String = description

  def getInterfaceChain: InterfaceInteractionChain =
    synchronized {
      interfaceChain match {
        case Some(value) =>
          value
        case None =>
          interfaceChain = Some(InterfaceInteractionChain.get(this))
          interfaceChain.get
      }
    }

  def getApp: MobileApp = app

  def getStart: ZonedDateTime = start

  def getEnd: ZonedDateTime = end

  def isSuccess: Boolean = success

  def getAppPreferences: List[AppPreferences] =
    synchronized {
      appPreferences match {
        case Some(value) =>
          value
        case None =>
          appPreferences = Some(AppPreferences.get(this))
          appPreferences.get
      }
    }

  def getTrafficCollections: List[TrafficCollection] =
    synchronized {
      trafficCollections match {
        case Some(value) =>
          value
        case None =>
          trafficCollections = Some(
            TrafficCollectionCache.getTrafficCollections(this.getExperimentId, this.getId)
          )
          trafficCollections.get
      }
    }

  def getId: Int = id

  def getExperimentId: Int = experiment

  def getInterfaceErrors: List[InterfaceAnalysisError] =
    synchronized {
      interfaceErrors match {
        case Some(value) =>
          value
        case None =>
          interfaceErrors = Some(
            InterfaceAnalysisError.getInterfaceAnalysisErrors(List(this.getId))
          )
          interfaceErrors.get
      }
    }

  def getExperimentErrors: List[ExperimentError] = {
    synchronized {
      this.experimentErrors match {
        case Some(errors) =>
          errors
        case None =>
          this.experimentErrors = Some(ExperimentError.getExperimentErrors(this))
          this.experimentErrors.get
      }
    }
  }

  def toJson: JsValue = throw new NotImplementedError()

}

object InterfaceAnalysis {

  def get(experiment: Experiment, app: String)(implicit
      database: Database
  ): List[InterfaceAnalysis] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     experiment,
                     app_id,
                     app_version,
                     app_os,
                     description,
                     start_time,
                     end_time,
                     success
              FROM interfaceanalysis
              WHERE experiment = ${experiment.getId} AND
                    app_id = $app
           """.map(InterfaceAnalysis.apply).toList.apply()
    }
  }

  def apply(entity: WrappedResultSet)(implicit database: Database): InterfaceAnalysis = {
    new InterfaceAnalysis(
      entity.int("id"),
      entity.int("experiment"),
      MobileApp(entity),
      entity.string("description"),
      entity.zonedDateTime("start_time"),
      entity.zonedDateTime("end_time"),
      entity.boolean("success")
    )
  }

  def getLatestSuccessfulAnalyses(experiment: Experiment): List[InterfaceAnalysis] = {
    get(experiment)
      .filter(_.isSuccess)
      .groupBy(_.getApp)
      .view
      .mapValues(analyses =>
        analyses.reduce((a1, a2) =>
          if (a1.getStart.isAfter(a2.getStart))
            a1
          else
            a2
        )
      )
      .values
      .toList
  }

  def get(experiment: Experiment)(implicit database: Database): List[InterfaceAnalysis] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     experiment,
                     app_id,
                     app_version,
                     app_os,
                     description,
                     start_time,
                     end_time,
                     success
              FROM interfaceanalysis
              WHERE experiment = ${experiment.getId}
           """.map(InterfaceAnalysis.apply).toList.apply()
    }
  }

}
