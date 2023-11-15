package de.tubs.cs.ias.plotalyzer.database.entities.exodus

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc._

case class AppTracker(appId: String, versionCode: Int, trackerId: Int, experimentId: Int)

object AppTracker extends SQLSyntaxSupport[AppTracker] {
  override val connectionPoolName: String = Database.POOL_NAME
  override val schemaName: Option[String] = Some("pluginexodus")
  override val tableName = "apptracker"

  def getAll(implicit db: Database): List[AppTracker] = {
    db.withDatabaseSession { implicit session =>
      val at = AppTracker.syntax("at")
      withSQL {
        select(at.*).from(AppTracker as at)
      }.map(AppTracker.apply).list.apply()
    }
  }

  def apply(resultSet: WrappedResultSet): AppTracker = {
    AppTracker(
      resultSet.string("app_id"),
      resultSet.int("version_code"),
      resultSet.int("tracker_id"),
      resultSet.int("experiment_id")
    )
  }

  def insert(appTracker: AppTracker)(implicit db: Database): Boolean = {
    val count = synchronized {
      db.withDatabaseSession { implicit session =>
        applyUpdate {
          insertInto(AppTracker)
            .values(appTracker.appId, appTracker.versionCode, appTracker.trackerId, appTracker.experimentId)
        }
      }
    }
    count == 1
  }
}
