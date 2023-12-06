package de.tubs.cs.ias.plotalyzer.database.entities.adblock

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc._

case class RequestRecipient(requestId: Int, developer: Boolean)

object RequestRecipient extends SQLSyntaxSupport[RequestRecipient] {
  override val connectionPoolName: String = Database.POOL_NAME
  override val schemaName: Option[String] = Some("pluginadblock")
  override val tableName = "requestrecipient"

  def getRequestRecipients(requestIds: List[Int])(implicit db: Database): List[RequestRecipient] = {
    db.withDatabaseSession { implicit session =>
      val rc = RequestRecipient.syntax("rc")
      withSQL {
        select(rc.*).from(RequestRecipient as rc).where.in(rc.requestId, requestIds)
      }.map(RequestRecipient.apply).list.apply()
    }
  }

  def apply(resultSet: WrappedResultSet): RequestRecipient = {
    RequestRecipient(resultSet.int("request_id"), resultSet.boolean("developer"))
  }

  def getAll(implicit db: Database): List[RequestRecipient] = {
    db.withDatabaseSession { implicit session =>
      val rc = RequestRecipient.syntax("rc")
      withSQL {
        select(rc.*).from(RequestRecipient as rc)
      }.map(RequestRecipient.apply).list.apply()
    }
  }

  def insert(recipient: RequestRecipient)(implicit db: Database): Boolean = {
    val count = synchronized {
      db.withDatabaseSession { implicit session =>
        applyUpdate {
          insertInto(RequestRecipient).values(recipient.requestId, recipient.developer)
        }
      }
    }
    count == 1
  }

  def update(recipient: RequestRecipient)(implicit db: Database): Boolean = {
    val count = db.withDatabaseSession { implicit session =>
      withSQL {
        QueryDSL
          .update(RequestRecipient)
          .set(RequestRecipient.column.developer -> recipient.developer)
          .where
          .eq(RequestRecipient.column.requestId, recipient.requestId)
      }.update.apply()
    }
    count == 1
  }

}
