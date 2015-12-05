package controllers

import models.User
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global

object LookupGoogle extends Translator {
  val name = "Google Translate"
  val expiration = Utils.getExpiration("google")
  val codeFormat = 'googleCodes
  val googleKey = configuration.getString("google.key")

  /**
   * Endpoint for translating via Google
   */
  def translate(user: User, src: String, dest: String, text: String) = {
    googleKey.flatMap { key =>
      val query = WS.url("https://www.googleapis.com/language/translate/v2")
        .withQueryString("source" -> src, "target" -> dest, "q" -> text, "key" -> key).get()
      val result = Await.result(query, Duration.Inf)
      if(result.status != 200) None
      else {
        val translations = (result.json \ "data" \ "translations" \\ "translatedText").map(_.toString)
        if(translations.length > 0) Some(translations)
        else None
      }
    }
  }
}