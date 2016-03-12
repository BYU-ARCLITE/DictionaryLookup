package controllers

import models.User
import Utils._
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.mvc._
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

  def getPairs = for {
    from <- LangCodes.googleCodes
	to <- LangCodes.googleCodes
  } yield (from, to)

  /**
   * Endpoint for translating via Google
   */
  def translate(user: User, src: String, dest: String, text: String)
               (implicit request: RequestHeader, restart: TRestart) = {
    googleKey.flatMap { key =>
      val query = WS.url("https://www.googleapis.com/language/translate/v2")
        .withQueryString("source" -> src, "target" -> dest,
                         "q" -> text, "key" -> key).get()

      val result = Await.result(query, Duration.Inf)
      if(result.status != 200) None
      else {
        val trans = (result.json \\ "translatedText")
        if(trans.length == 0) None
        else {
          val imgURL = routes.Assets.at("images/google-translate.png")
                .absoluteURL(configuration.getBoolean("HTTPS").getOrElse(false))

          val attribution = Json.obj(
            "name" -> name,
            "attribution" -> s"""<a href="http://translate.google.com"><img src="$imgURL"/></a>"""
          )

          val result = Json.obj(
            "translations" -> trans.map { jsstr =>
              Json.obj(
                "text" -> quoteExpr.replaceAllIn(Utils.unescape(jsstr.toString), ""),
                "source" -> attribution
              )
            }
          )

          Some(result)
        }
      }
    }
  }
}