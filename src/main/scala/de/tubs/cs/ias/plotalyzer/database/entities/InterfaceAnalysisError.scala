package de.tubs.cs.ias.plotalyzer.database.entities

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.utility.StackTrace
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}
import spray.json.{JsNull, JsNumber, JsObject, JsString, JsValue}

class InterfaceAnalysisError(
  id: Int,
  analysis: Int,
  interface: Option[Int],
  message: String,
  stackTrace: String
) extends AppAnalyzerError {

  def getAnalysisId: Int = analysis

  def toJson: JsValue = {
    JsObject(
      "id" -> JsNumber(getId),
      "analysis" -> JsNumber(analysis),
      "interface" ->
        (getInterfaceId match {
          case Some(id) => JsNumber(id)
          case None     => JsNull
        }),
      "message" -> JsString(getMessage),
      "cause" ->
        (getStackTrace.getFirst("de.tubs".r) match {
          case Some(hit) => JsString(hit)
          case None      => JsString(getStackTrace.trace)
        })
    )
  }

  def getId: Int = id

  def getInterfaceId: Option[Int] = interface

  def getMessage: String = message

  def getStackTrace: StackTrace = StackTrace(stackTrace)
}

object InterfaceAnalysisError {

  def getInterfaceAnalysisErrors(
      analysis: List[Int]
  )(implicit database: Database): List[InterfaceAnalysisError] = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     analysis,
                     interface,
                     message,
                     stacktrace
              FROM interfaceanalysiserror
              WHERE analysis IN ($analysis)
           """.map(InterfaceAnalysisError.apply).toList.apply()
    }
  }

  def getInterfaceAnalysisError(interfaceAnalysisId: Int)(implicit database: Database): InterfaceAnalysisError = {
    database.withDatabaseSession { implicit session =>
      sql"""SELECT id,
                     analysis,
                     interface,
                     message,
                     stacktrace
              FROM interfaceanalysiserror
              WHERE id = $interfaceAnalysisId
           """.map(InterfaceAnalysisError.apply).first.apply()
        .getOrElse(throw new RuntimeException(s"there is no interface analysis error with id $interfaceAnalysisId"))
    }
  }

  def apply(entity: WrappedResultSet): InterfaceAnalysisError = {
    new InterfaceAnalysisError(
      entity.int("id"),
      entity.int("analysis"),
      entity.intOpt("interface"),
      entity.string("message"),
      entity.string("stacktrace")
    )
  }

}
