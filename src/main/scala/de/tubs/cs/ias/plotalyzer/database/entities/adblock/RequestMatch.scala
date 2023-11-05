package de.tubs.cs.ias.plotalyzer.database.entities.adblock

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc._

case class RequestMatch(requestId: Int, listId: Int, isMatch: Boolean)

object RequestMatch extends SQLSyntaxSupport[RequestMatch] {
  override val connectionPoolName: String = Database.POOL_NAME
  override val schemaName: Option[String] = Some("pluginadblock")
  override val tableName = "requestmatch"

  def getRequestMatches(requestIds: List[Int], filterListIds: List[Int])(implicit
      db: Database
  ): List[RequestMatch] = {
    db.withDatabaseSession { implicit session =>
      val rm = RequestMatch.syntax("rm")
      withSQL {
        select(rm.*)
          .from(RequestMatch as rm)
          .where
          .in(rm.requestId, requestIds)
          .and
          .in(rm.listId, filterListIds)
      }.map(RequestMatch.apply).list.apply()
    }
  }

  def apply(resultSet: WrappedResultSet): RequestMatch = {
    RequestMatch(resultSet.int("request_id"), resultSet.int("list_id"), resultSet.boolean("match"))
  }

}
