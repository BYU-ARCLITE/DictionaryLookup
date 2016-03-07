package models

import anorm._
import play.api.db.DB
import play.api.Play.current

object Service {
  val tableName = "serviceList"

  def setByUser(user: User, services: List[String]) = DB.withConnection {
    implicit connection =>
      SQL(s"delete from $tableName where userId = ${user.id.get}").execute()
      val arglist = services.zipWithIndex.map { case (name, i) =>
        List[NamedParameter]('id -> user.id, 'priority -> i, 'name -> name)
      }
      if(!arglist.isEmpty) BatchSql(
        s"insert into $tableName (userId, priority, name) values ({id}, {priority}, {name})",
        arglist.head, arglist.tail:_*
      ).execute()
  }

  /**
   * Lists a user's notifications
   * @param user The user who made the notification belong to
   * @return The list of notifications
   */
  def listByUser(user: User): List[String] = DB.withConnection {
    implicit connection =>
      if (!user.id.isEmpty) {
        SQL(s"select name from $tableName where userId = ${user.id.get} order by priority")
        .fold(List[String]()) { (l, row) => l :+ row[String]("name") }
        .fold(_ => List[String](), l => l)
      } else Nil
  }

}