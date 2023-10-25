package de.tubs.cs.ias.plotalyzer.database

import de.halcony.argparse.ParsingResult
import de.tubs.cs.ias.plotalyzer.database.Database.connectionPools
import scala.collection.mutable.HashSet
import scalikejdbc.{ConnectionPool, ConnectionPoolSettings, DB, DBSession, using}

class Database(val poolName: String) {
  def withDatabaseSession[T](func: DBSession => T): T = {
    if (connectionPools.contains(poolName)) {
      using(ConnectionPool(poolName).borrow()) { con =>
        DB(con).localTx { session => func(session) }
      }
    } else {
      throw new RuntimeException(
        s"there is no '$poolName' postgres connection pool, initialize first"
      )
    }
  }
}

object Database {
  private val POOL_NAME = "plotalyzer"
  private val connectionPools: HashSet[String] = new HashSet[String]

  implicit def default: Database = new Database(POOL_NAME)

  def get(poolName: String): Database = new Database(poolName)

  def initialize(pargs: ParsingResult): Unit = {
    val conf = DatabaseConf.read(pargs.getValue[String]("database-conf"))
    Database.apply(conf, POOL_NAME)
  }

  def apply(conf: DatabaseConf, poolName: String): Database = {
    initializeConnectionPool(conf.host, conf.port, conf.user, conf.pwd, conf.database, poolName)
    new Database(poolName)
  }

  private def initializeConnectionPool(
      host: String,
      port: String,
      user: String,
      pwd: String,
      database: String,
      poolName: String
  ): Unit = {
    if (!connectionPools.contains(poolName)) {
      val settings: ConnectionPoolSettings =
        ConnectionPoolSettings(initialSize = 10, maxSize = 10, driverName = "org.postgresql.Driver")
      val url = s"jdbc:postgresql://$host:$port/$database"
      ConnectionPool.add(poolName, url, user, pwd, settings)
      connectionPools.add(poolName)
    }
  }
}
