package de.tubs.cs.ias.plotalyzer.database.entities.exodus

import de.tubs.cs.ias.plotalyzer.database.Database
import de.tubs.cs.ias.plotalyzer.json.ExodusTracker
import scalikejdbc._

case class Tracker(id: Int, name: String)

object Tracker extends SQLSyntaxSupport[Tracker] {
  override val connectionPoolName: String         = Database.POOL_NAME
  override val schemaName        : Option[String] = Some("pluginexodus")
  override val tableName                          = "tracker"

  def getAll(implicit db: Database): List[Tracker] = {
    db.withDatabaseSession { implicit session =>
      val t = Tracker.syntax("t")
      withSQL {
        select(t.*).from(Tracker as t)
      }.map(Tracker.apply).list.apply()
    }
  }

  def apply(resultSet: WrappedResultSet): Tracker = {
    Tracker(resultSet.int("id"), resultSet.string("name"))
  }

  def apply(tracker: ExodusTracker): Tracker = {
    Tracker(tracker.id, tracker.name)
  }

  def insert(tracker: Tracker)(implicit db: Database): Boolean = {
    val count = synchronized {
      db.withDatabaseSession { implicit session =>
        applyUpdate {
          insertInto(Tracker).values(tracker.id, tracker.name)
        }
      }
    }
    count == 1
  }
}
