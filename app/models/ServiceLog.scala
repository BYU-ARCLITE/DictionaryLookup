package models

import anorm.SQL
import play.api.db.DB
import play.api.Play.current

object ServiceLog {
  val tableName = "serviceLog"

  def record(user: User, src: String, dst: String, txt: String, name: String, used: Boolean) = DB.withConnection {
    implicit connection =>
      anorm.SQL("insert into " + tableName + "(userId, src, dst, txt, service, used) values ({id}, {src}, {dst}, {txt}, {name}, {used})")
        .on('id -> user.id, 'src -> src, 'dst -> dst, 'txt -> txt, 'name -> name, 'used -> used)
        .execute()
  }
}