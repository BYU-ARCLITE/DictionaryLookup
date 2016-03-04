package controllers

import models.User
import Utils._
import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global

object LookupWordReference extends Translator {
  //See http://www.wordreference.com/docs/api.aspx
  val name = "WordReference"
  val expiration = Utils.getExpiration("wordReference")
  val codeFormat = 'iso639_1

  val wordReferenceKey = configuration.getString("wordReference.key")
  val whitespace = "\\s".r

  val codeMap = Map(
    "ar" -> "ar", //Arabic
    "zh" -> "zh", //Chinese
    "cs" -> "cz", //Czech
    "en" -> "en", //English
    "fr" -> "fr", //French
    "el" -> "gr", //Greek
    "ja" -> "ja", //Japanese
    "ko" -> "ko", //Korean
    "pl" -> "pl", //Polish
    "pt" -> "pt", //Portuguese
    "ro" -> "ro", //Romanian
    "es" -> "es", //Spanish
    "tr" -> "tr" //Turkish
  )

  val PartsList = List("Entries","PrincipalTranslations","AdditionalTranslations")

  def mapEntries(json: JsObject)(body: (JsObject => JsObject)) = {
    val terms = json.fields.collect {
      case (key, obj:JsObject) if key.startsWith("term") => obj
    }

    terms.flatMap { obj =>
      PartsList.flatMap { pname =>
        (obj \ pname).asOpt[JsObject] match {
        case Some(part) =>
          part.fields.collect { case (_, entry:JsObject) => entry }
        case _ => Nil
        }
      }
    }.map(body)
  }

  val TranslationList = List("FirstTranslation", "SecondTranslation", "ThirdTranslations", "FourthTranslation")

  def getTranslationsIn(entry: JsObject) = TranslationList.flatMap { name =>
    (entry \ name \ "term") match {
      case translation:JsString => List(translation.as[String])
      case _ => Nil
    }
  }

  def requestEntries(key: String, scode: String, dcode: String, text: String) = {
    val url = "http://api.wordreference.com/0.8/" + URLEncoder.encode(s"$key/json/$scode$dcode/$text", "UTF-8")
    val result = Await.result(WS.url(url).get(), Duration.Inf)
    if(result.status != 200) None
    else {
      val lemmas = mapEntries(result.json.as[JsObject]) { entry =>
        val original = entry \ "OriginalTerm"
        val pos = (original \ "POS").asOpt[String]
                  .collect { case str if str.length > 0 => str }

        var lemma = Json.obj(
          "representations" -> Json.arr("Orthographic"),
          "lemmaForm" -> "lemma",
          "forms" -> Json.obj(
            "lemma" -> Json.obj(
              "Orthographic" -> Seq((original \ "term").as[String])
            )
          ),
          "senses" -> TranslationList.flatMap { name =>
            (entry \ name).asOpt[JsObject].map { trans =>
              val term = (trans \ "term").as[String]
              val sense = (trans \ "sense").as[String]
              Json.obj("definition" -> s"$term ($sense)")
            }
          },
          "sources" -> Seq(name)
        )

        if (pos.isDefined) {
          lemma ++ Json.obj("pos" -> pos.get)
        } else lemma
      }

      if (lemmas.size == 0) None
      else Some(lemmas)
    }
  }

  /**
   * Endpoint for translating via WordReference
   */
  def translate(user: User, src: String, dst: String, text: String)
               (implicit restart: TRestart): Option[JsObject] = {

    if (!wordReferenceKey.isDefined) return None
    if (whitespace.findFirstIn(text).isDefined) return None

    val scode = codeMap.get(src)
    val dcode = codeMap.get(dst)

    val hasEnglish = (scode, dcode) match {
    case (Some("en"),Some(_)) => true
    case (Some(_),Some("en")) => true
    case _ => false
    }

    if(!hasEnglish) return None

    requestEntries(wordReferenceKey.get, scode.get, dcode.get, text)
      .map { lemmas =>
        Json.obj(
          //"translations" -> Json.arr("free translation text")
          "words" -> Json.arr(
            Json.obj(
              "start" -> 0,
              "end" -> text.length,
              "lemmas" -> lemmas
            )
          )
        )
      }
  }
}