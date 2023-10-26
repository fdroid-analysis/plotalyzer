package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.utility.{StackTrace, Time}
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import spray.json.{JsNumber, JsObject, JsString, JsValue}
import java.time.ZonedDateTime

class ExperimentError(id: Int, time: ZonedDateTime, message: String, stacktrace: String) extends AppAnalyzerError {

  def toJson: JsValue = {
    JsObject(
      "id" -> JsNumber(getId),
      "time" -> JsString(Time.format(getTime)),
      "message" -> JsString(getMessage),
      "cause" ->
        (getStackTrace.getFirst("de.tubs.".r) match {
          case Some(hit) =>
            JsString(hit)
          case None =>
            JsString(getStackTrace.trace)
        })
    )
  }

  def getId: Int = id

  def getTime: ZonedDateTime = time

  def getMessage: String = message

  def getStackTrace: StackTrace = StackTrace(stacktrace)

}

object ExperimentError {

  def getExperimentErrors(experiment: Int)(implicit database: Database): List[ExperimentError] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     time,
                     experiment,
                     message,
                     stacktrace
              FROM experimenterror
              WHERE experiment = $experiment""".map(ExperimentError.apply).toList.apply()
    }
  }

  def apply(entity: WrappedResultSet): ExperimentError = {
    new ExperimentError(
      entity.int("id"),
      entity.zonedDateTime("time"),
      entity.string("message"),
      entity.string("stacktrace")
    )
  }

  def getExperimentError(id: Int)(implicit database: Database): ExperimentError = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     time,
                     experiment,
                     message,
                     stacktrace
              FROM experimenterror
              WHERE id = $id
           """
        .map(ExperimentError.apply)
        .first
        .apply()
        .getOrElse(throw new RuntimeException(s"there is no experiment error with id $id"))
    }
  }

  def getExperimentErrors(
      analysis: InterfaceAnalysis
  )(implicit database: Database): List[ExperimentError] = {
    val startTime = analysis.getStart.toOffsetDateTime
    val appId = s"%${analysis.getApp.id}%"
    val errors = database.withDatabaseSession { implicit session =>
      val sql =  sql"""SELECT ee.* FROM experimenterror ee
            WHERE ee.time::timestamp BETWEEN $startTime::timestamp - INTERVAL '1 minute' AND $startTime::timestamp + INTERVAL '1 minute'
              AND ee.message LIKE $appId;
        """
      val mapped = sql.map(ExperimentError.apply)
      val list = mapped.toList
      val applied = list.apply()
      applied
    }
    errors
  }

}
