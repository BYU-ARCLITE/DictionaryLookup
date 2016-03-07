package controllers

import models.User
import Utils._
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
  val quoteExpr = """^\s*"\s*|\s*"\s*$""".r

  /**
   * Endpoint for translating via Google
   * TODO: Extract word-and-phrase alignment information
   */
  def translate(user: User, src: String, dest: String, text: String)
               (implicit restart: TRestart) = {
    googleKey.flatMap { key =>
      val query = WS.url("https://www.googleapis.com/language/translate/v2")
        .withQueryString("source" -> src, "target" -> dest,
                         "q" -> text, "key" -> key).get()
      val result = Await.result(query, Duration.Inf)
      if(result.status != 200) None
      else {
        val translations = (result.json \ "data" \ "translations" \\ "translatedText")
            .map { jsstr =>
              Json.obj(
                "text" -> quoteExpr.replaceAllIn(Utils.unescape(jsstr.toString), ""),
                "source" -> Json.obj("name" -> name, "attribution" -> s"<i>$name</i>")
              )
            }
        if(translations.length > 0) Some(translations)
        else None
      }
    }.map { translations =>
      Json.obj("translations" -> translations)
    }
  }
}