package de.tubs.cs.ias.plotalyzer.database.entities.adblock

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc.{WrappedResultSet, scalikejdbcSQLInterpolationImplicitDef}

case class FilterList(id: Int, name: String)

object FilterList {

  def getFilterLists(ids: List[Int])(implicit db: Database): List[FilterList] = {
    db.withDatabaseSession { implicit session =>
      sql"SELECT * FROM pluginadblock.filterlist WHERE id IN ($ids)"
        .map(FilterList.apply)
        .toList
        .apply()
    }
  }

  def apply(resultSet: WrappedResultSet): FilterList = {
    FilterList(resultSet.int("id"), resultSet.string("name"))
  }

  def getName(id: Int)(implicit db: Database): Option[String] = {
    val filterList = db.withDatabaseSession { implicit session =>
      sql"SELECT * FROM pluginadblock.filterlist WHERE id = $id"
        .map(FilterList.apply)
        .toOption
        .apply()
    }
    filterList match {
      case Some(filterList) =>
        Some(filterList.name)
      case _ =>
        None
    }
  }

  def getAll(implicit db: Database): List[FilterList] = {
    db.withDatabaseSession { implicit session =>
      sql"SELECT * FROM pluginadblock.filterlist".map(FilterList.apply).toList.apply()
    }
  }
}
