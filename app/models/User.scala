package models

import anorm.{Id, NotAssigned, ~, Pk, SQL, ParameterValue}
import anorm.SqlParser._
import play.api.db.DB
import play.api.Play.{current, configuration}
import play.api.Logger
import controllers.routes
import org.mindrot.jbcrypt.BCrypt
import java.security.SecureRandom

/**
 * User
 */
case class User(id: Pk[Long], username: String, passHash: String, email: String, authKey: String) {

  def insert(tablename: String, fields: (Symbol, ParameterValue[_]) *): Pk[Long] = {
    val fieldNames = fields.map(_._1.name).mkString(", ")
    val fieldValues = fields.map("{" + _._1.name + "}").mkString(", ")

    DB.withConnection {
      implicit connection =>
        val id: Option[Long] = SQL("insert into "+tablename+" ("+fieldNames+") values ("+fieldValues+")")
          .on(fields: _*).executeInsert()
        Id(id.get)
    }
  }

  def update(tablename: String, fields: (Symbol, ParameterValue[_]) *) {
    assert(fields.map(_._1.name).contains("id"))
    val fieldEntries = fields.map(_._1.name).filterNot(_ == "id").map(n => n + " = {" + n + "}").mkString(", ")

    DB.withConnection {
      implicit connection =>
        SQL("update "+tablename+" set "+fieldEntries+" where id = {id}").on(fields: _*).executeUpdate()
    }
  }

  /**
   * Saves the user to the DB
   * @return The possibly updated user
   */
  def save: User = if(id.isDefined){
    update(User.tableName, 'id -> id, 'username -> username,
      'passHash -> passHash, 'email -> email, 'authKey -> authKey)
    this
  } else {
    val nid = insert(User.tableName, 'id -> id, 'username -> username,
      'passHash -> passHash, 'email -> email, 'authKey -> authKey)
    this.copy(nid)
  }

  /**
   * Deletes the user from the DB
   */
  def delete = DB.withConnection {
    implicit connection =>
      SQL("delete from " + User.tableName + " where id = {id}").on('id -> id).execute()
  }

  def checkpw(plain: String) = BCrypt.checkpw(plain, passHash)

  def setKey(): User = {
    this.copy(authKey = User.uniqueKey).save
  }

  def setPass(password: String): User = {
    val hash = BCrypt.hashpw(password, BCrypt.gensalt(12))
    this.copy(passHash = hash, authKey = User.uniqueKey).save
  }

  def setEmail(email: String): User = {
    this.copy(email = email).save
  }

  /**
   * Any items that are retrieved from the DB should be cached here in order to reduce the number of DB calls
   */
  val cacheTarget = this
  object cache {
    var cachedServices: Option[List[String]] = None

    def getServices = {
      if (cachedServices.isEmpty)
        cachedServices = Some(Service.listByUser(cacheTarget))
      cachedServices.get
    }

    def setServices(services: List[String]) = {
      Service.setByUser(cacheTarget, services)
      cachedServices = Some(services)
    }
  }

  /**
   * Gets a the user's service preference list
   * @return
   */
  def getServices: List[String] = cache.getServices

  def setServices = cache.setServices _

}

object User {
  val random = new SecureRandom()
  val tableName = "userAccount"

  import collection.JavaConversions._
  val defaultServices = configuration.getStringList("user.servicelist").map(_.toList)
    .getOrElse(List("BYUDictionaries", "WordReference", "GoogleTranslate"))

  val simple = {
    get[Pk[Long]](tableName + ".id") ~
      get[String](tableName + ".username") ~
      get[String](tableName + ".passHash") ~
      get[String](tableName + ".email") ~
      get[String](tableName + ".authKey") map {
      case id ~ username ~ passHash ~ email ~ authKey => {
        User(id, username, passHash, email, authKey)
      }
    }
  }

  /**
   * Search the DB for a user with the given id.
   * @param id The id of the user.
   * @return If a user was found, then Some[User], otherwise None
   */
  def findById(id: Long): Option[User] = DB.withConnection {
    implicit connection =>
      anorm.SQL("select * from " + tableName + " where id = {id}")
        .on('id -> id).as(simple.singleOpt)
  }

  /**
   * Search the DB for a user with the given authentication info
   * @param authKey the API key
   * @return If a user was found, then Some[User], otherwise None
   */
  def findByKey(authKey: String): Option[User] = DB.withConnection {
    implicit connection =>
      anorm.SQL("select * from userAccount where authKey = {authKey}")
        .on('authKey -> authKey).as(simple.singleOpt)
  }

  /**
   * Finds a user based on the username
   * @param username The username to look for
   * @return If a user was found, then Some[User], otherwise None
   */
  def findByUsername(username: String): Option[User] = DB.withConnection {
    implicit connection =>
      anorm.SQL("select * from userAccount where username = {username}")
        .on('username -> username).as(simple.singleOpt)
  }

  def uniqueKey = {
    var key = BigInt(128, random).toString(16)
    while(findByKey(key).isDefined) {
      key = BigInt(128, random).toString(16)
    }
    key
  }

  def create(username: String, password: String, email: String): Option[User] = {
    // Check if the user is already created
    if(findByUsername(username).isDefined) None
    else {
      val hash = BCrypt.hashpw(password, BCrypt.gensalt(12))
      val user = User(NotAssigned, username, hash, email, uniqueKey).save
      user.setServices(defaultServices)
      Some(user)
    }
  }

  /**
   * Gets all users in the DB
   * @return The list of users
   */
  def list: List[User] = DB.withConnection {
    implicit connection =>
      anorm.SQL("select * from " + tableName).as(simple *)
  }
}