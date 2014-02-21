package models

import anorm.{SQL, toParameterValue}
import play.api.db.DB
import play.api.Play.current

object Service {
  val tableName = "serviceList"

  def setByUser(user: User, services: List[String]) = DB.withConnection {
    implicit connection =>
      anorm.SQL("delete from " + tableName + " where userId = {id}").on('id -> user.id).execute()
      val arglist = services.zipWithIndex.map { case (name, i) =>
        List( "id" -> toParameterValue(user.id),
              "priority" -> toParameterValue(i),
              "name" -> toParameterValue(name) )
      }
      val query = anorm.SQL("insert into " + tableName + "(userId, priority, name) values ({id}, {priority}, {name})")
      query.asBatch.addBatchList(arglist).execute()
  }

  /**
   * Lists a user's notifications
   * @param user The user who made the notification belong to
   * @return The list of notifications
   */
  def listByUser(user: User): List[String] = DB.withConnection {
    implicit connection =>
      val query = anorm.SQL("select name from " + tableName + " where userId = {id} order by priority")
        .on('id -> user.id)
      query().map(row => row[String]("name")).toList
  }

}