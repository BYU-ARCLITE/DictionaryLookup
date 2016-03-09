package controllers

import models.User
import Utils._
import edu.byu.arclite.dictionary.DictionaryCache
import play.api.mvc._
import play.api.libs.json._

object LookupBYU extends Translator {

  val name = "BYU Dictionaries"
  val expiration = Utils.getExpiration("byu")
  val codeFormat = 'iso639_3

  /**
   * Endpoint for translating via BYU Dictionaries
   */
  def translate(user: User, src: String, dst: String, text: String)
               (implicit request: RequestHeader, restart: TRestart) = {
    val key = s"$src-$dst"
    DictionaryCache.getDictionaryEntry(key, text).map { definition =>
      Json.obj(
        "words" -> Json.arr(
          Json.obj(
            "start" -> 0,
            "end" -> text.length,
            "lemmas" -> Json.arr(
              Json.obj(
                "representations" -> Json.arr("Orthographic"),
                "lemmaForm" -> "lemma",
                "forms" -> Json.obj(
                  "lemma" -> Json.obj(
                    "Orthographic" -> Json.arr(text)
                  )
                ),
                "senses" -> Json.arr(
                  Json.obj("definition" -> definition)
                ),
                "sources" -> Json.arr(
                  Json.obj("name" -> name, "attribution" -> s"<i>$name</i>")
                )
              )
            )
          )
        )
      )
    }
  }
}