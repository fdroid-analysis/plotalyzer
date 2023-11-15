package de.tubs.cs.ias.plotalyzer.database.entities.exodus

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc._

case class AppLibrary(appId: String, versionCode: Int, libraryName: String, experimentId: Int)

object AppLibrary extends SQLSyntaxSupport[AppLibrary] {
  override val connectionPoolName: String = Database.POOL_NAME
  override val schemaName: Option[String] = Some("pluginexodus")
  override val tableName = "applibrary"

  def getAll(implicit db: Database): List[AppLibrary] = {
    db.withDatabaseSession { implicit session =>
      val al = AppLibrary.syntax("al")
      withSQL {
        select(al.*).from(AppLibrary as al)
      }.map(AppLibrary.apply).list.apply()
    }
  }

  def getByExperiment(experimentId: Int)(implicit db: Database): List[AppLibrary] = {
    db.withDatabaseSession { implicit session =>
      val al = AppLibrary.syntax("al")
      withSQL {
        select(al.*).from(AppLibrary as al).where.eq(al.experimentId, experimentId)
      }.map(AppLibrary.apply).list.apply()
    }
  }

  def apply(resultSet: WrappedResultSet): AppLibrary = {
    AppLibrary(
      resultSet.string("app_id"),
      resultSet.int("version_code"),
      resultSet.string("library"),
      resultSet.int("experiment_id")
    )
  }

  def insert(appLibrary: AppLibrary)(implicit db: Database): Boolean = {
    val count = synchronized {
      db.withDatabaseSession { implicit session =>
        applyUpdate {
          insertInto(AppLibrary).values(
            appLibrary.appId,
            appLibrary.versionCode,
            appLibrary.libraryName,
            appLibrary.experimentId
          )
        }
      }
    }
    count == 1
  }

}
