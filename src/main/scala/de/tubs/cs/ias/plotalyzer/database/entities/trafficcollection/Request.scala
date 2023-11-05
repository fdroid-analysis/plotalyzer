package de.tubs.cs.ias.plotalyzer.database.entities.trafficcollection

import de.tubs.cs.ias.plotalyzer.database.Database
import scalikejdbc._

import java.time.ZonedDateTime

case class Request(
    id: Int,
    run: Int,
    startTime: ZonedDateTime,
    scheme: String,
    method: String,
    host: String,
    port: String,
    path: String,
    content: String,
    contentRaw: Array[Byte],
    authority: String,
    error: Option[String],
    cookies: List[Cookie],
    headers: List[Header],
    trailers: List[Trailer]
) {

  def getHost: String =
    Option(host) match {
      case Some(value) =>
        value
      case None =>
        ""
    }

  def getUrl: String = s"$scheme://$host$getPath"

  def getPath: String =
    Option(path) match {
      case Some(value) =>
        value.split('?').head
      case None =>
        ""
    }

  def getFullUrl: String = s"$scheme://$host$getPathWithQuery"

  def getPathWithQuery: String =
    Option(path) match {
      case Some(value) =>
        value
      case None =>
        ""
    }

}

object Request extends SQLSyntaxSupport[Request] {
  override val connectionPoolName: String = Database.POOL_NAME

  def getRequestTable(runs: List[Int])(implicit database: Database): Map[Int, List[Request]] = {
    val fragments: List[Fragment] = database.withDatabaseSession { implicit session =>
      val r = Request.syntax("r")
      withSQL {
        select(
          r.id,
          r.run,
          r.startTime,
          r.scheme,
          r.method,
          r.host,
          r.port,
          r.path,
          r.content,
          r.contentRaw,
          r.authority,
          r.error
        ).from(Request as r).where.in(r.run, runs)
      }.map(Fragment.apply).list.apply()
    }
    val requests = Request.fromFragments(fragments)
    requests.groupBy(_.run)
  }

  private def fromFragments(fragments: List[Fragment]): List[Request] = {
    val requestIds = fragments.map(_.id)
    val cookies: Map[Int, List[Cookie]] = Cookie.get(requestIds)
    val headers: Map[Int, List[Header]] = Header.get(requestIds)
    val trailers: Map[Int, List[Trailer]] = Trailer.get(requestIds)
    fragments.map { fragment =>
      Request(
        fragment,
        cookies.getOrElse(fragment.id, List()),
        headers.getOrElse(fragment.id, List()),
        trailers.getOrElse(fragment.id, List())
      )
    }
  }

  private def apply(
      fragment: Fragment,
      cookies: List[Cookie],
      headers: List[Header],
      trailers: List[Trailer]
  ): Request = {
    Request(
      fragment.id,
      fragment.run,
      fragment.start,
      fragment.scheme,
      fragment.method,
      fragment.host,
      fragment.port,
      fragment.path,
      fragment.content,
      fragment.contentRaw,
      fragment.authority,
      fragment.error,
      cookies,
      headers,
      trailers
    )
  }

  def getRequests(requestIds: List[Int])(implicit db: Database): List[Request] = {
    val fragments = db.withDatabaseSession { implicit session =>
      val r = Request.syntax("r")
      withSQL {
        select(
          r.id,
          r.run,
          r.startTime,
          r.scheme,
          r.method,
          r.host,
          r.port,
          r.path,
          r.content,
          r.contentRaw,
          r.authority,
          r.error
        ).from(Request as r).where.in(r.id, requestIds)
      }.map(Fragment.apply).list.apply()
    }
    Request.fromFragments(fragments)
  }

  private case class Fragment(
      id: Int,
      run: Int,
      start: ZonedDateTime,
      scheme: String,
      method: String,
      host: String,
      port: String,
      path: String,
      content: String,
      contentRaw: Array[Byte],
      authority: String,
      error: Option[String]
  )

  private object Fragment {
    def apply(entity: WrappedResultSet): Fragment = {
      Fragment(
        entity.int("id"),
        entity.int("run"),
        entity.zonedDateTime("start_time"),
        entity.string("scheme"),
        entity.string("method"),
        entity.string("host"),
        entity.string("port"),
        entity.string("path"),
        entity.string("content"),
        entity.bytes("content_raw"),
        entity.string("authority"),
        entity.stringOpt("error")
      )
    }
  }

}
