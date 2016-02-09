package controllers

import models.User
import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global

import play.api.Logger

object LookupSeaLang extends Translator {
  /**
   *  Russian Digital Reference Project Web API:
   *    http://www.sealang.net/russtest/api.htm 
   *
   * Example Requests
   *     EN-RU Dictionary: http://www.sealang.net/russtest/api.pl?service=dictionary&query=test&format=html&number=5&fold=yes&resource=l2l1
   *     RU-EN Dictionary: http://www.sealang.net/russtest/api.pl?service=dictionary&query=злой&format=html&number=5&fold=yes&resource=l1l2
   *     Russian Dictionary: http://www.sealang.net/russtest/api.pl?service=dictionary&query=злой&format=html&number=5&fold=yes&resource=all
   *     Russian Dictionary: http://www.sealang.net/russtest/api.pl?service=dictionary&query=злой&format=html&encode=unicode&number=5&fold=yes&return=syn&sort=down&rank=alpha&resource=all
   */

  val name = "SeaLang"
  val expiration = Utils.getExpiration("seaLang")
  val codeFormat = 'iso639_3

  /* Grabs all the direct Translations */
  def grabTranslations(json:Seq[JsValue]) : Seq[String] = {
    (for { entry <- json } yield {
      val word = (entry \ "$t").asOpt[String]
      word.getOrElse("").replaceAll("\\n", "<br/>").stripPrefix("<br/>").stripSuffix("<br/>")
    }).filterNot(_ == "")
  }

  /**
   * Tried to Implement English to Russian Translations, but the api returns a horrid response that is inconsistent
   * This function is here in case the api ever gets updated
   */
  def englishToRussian(user: User, src: String, dest: String, text: String) = {
    None
  }

 /**
  * Russian To English Translations
  * TODO: After evaluation by users, possibly remove if it is too cumbersome to view.
  *       Similar problem to function englishToRussian(src: String, dest: String, text: String)
  */
  def russianToEnglish(user: User, src: String, dest: String, text: String): Option[Seq[String]] = {
    val query = WS.url("http://www.sealang.net/russtest/api.pl")
    .withQueryString("service" -> "dictionary", "query" -> text,
	                 "format" -> "json", "phrase" -> text,
                     "number" -> "5", "fold" -> "yes",
					 "resource" -> "l1l2","encode" -> "unicode").get()
    val result = Await.result(query, Duration.Inf)
    val json = result.json.asOpt[JsObject]

    if(result.status != 200) None
    else {
      val entries = (json.get \ "return" \ "entry").as[Seq[JsValue]]

      if (entries.size == 0) None
      else {
        var definitions = grabTranslations(entries)
        val result = definitions /*.flatMap { defini =>
          if (defini.contains("см. также")) {
            val definit = if (defini.contains("v см. также")) {
              defini.replace("v см. также", "") 
            } else {
              defini.replace("см. также", "")
            }
            //dagan :+ russianToEnglish(src, dest, defini)
            Lookup.getFirst(user, src, dest, definit).get._3
          } else {
            Seq(defini)
          }
        }*/
      
        if (result.size != 0) {
          val temp = for { defin <- definitions } yield {
            "<b>" + text + ":</b><br/>" + defin
          }
          // Limit the translation to five definitions so that it does not get overbearing
          Some(temp.take(5))
        } else None
      }
    }
  }

  /**
   * Specifically Handles Russian Definitions
   */
  def russianToRussian(user: User, src: String, dest: String, text: String) = {
    val query = WS.url("http://www.sealang.net/russtest/api.pl")
    .withQueryString("service" -> "dictionary", "query" -> text,
                     "format" -> "json", "phrase" -> text,
                     "number" -> "5", "fold" -> "yes",
                     "encode" -> "unicode").get()
    val result = Await.result(query, Duration.Inf)
    val json = result.json.asOpt[JsObject]

    if(result.status != 200) None
    else {
      val entries = (json.get \ "return" \ "entry" \ "sense" \\ "def")

      if (entries.size == 0) None
      else {
        val definitions = grabTranslations(entries)
        if (definitions.size != 0) {
          var num = 0
          val temp = for { defin <- definitions } yield {
            num = num + 1
            "<b>" + num + ".)</b> " + defin
          }
          // Limit the translation to five definitions so that it does not get overbearing
          Some(temp.take(5))
        } else None
      }
    }
  }

  /**
   * Endpoint for translating via SeaLang
   */
  def translate(user: User, src: String, dst: String, text: String) =
    ((src, dst) match {
    case ("eng","rus") =>  // English to Russian Translation
      englishToRussian(user, src, dst, text)
    case ("rus", "eng") => // Russian to English Translation
      russianToEnglish(user, src, dst, text)
    case ("rus", "rus") => // Russian To Russian
      russianToRussian(user, src, dst, text)
    case _ => None
    }).map { translations =>
	  Json.obj(
	    //"translation" -> "free translation text"
	    "words" -> Json.arr(
	      Json.obj(
		    "start" -> 0,
		    "end" -> text.length,
		    "lemmas" -> translations.map { str =>
              Json.obj( // entry structure
                //"pos" -> "(string with POS info)",
                "representations" -> Json.arr("Cyrillic"),
                "lemmaForm" -> "lemma",
                "forms" -> Json.obj(
                  "lemma" -> Json.obj(
                    "Cyrillic" -> "(rep. value)"
                  )
                ),
                "senses" -> Json.arr(
                  Json.obj(
                    //"examples" -> Json.arr("list of example sentences in the source language"),
                    "definition" -> str
				  )
				)
			  )
			}
		  )
		)
	  )
	}
}