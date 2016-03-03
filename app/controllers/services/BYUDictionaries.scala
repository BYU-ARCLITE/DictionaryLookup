package controllers

import models.User
import Utils._
import edu.byu.arclite.dictionary.DictionaryCache
import play.api.libs.json._

object LookupBYU extends Translator {

  val name = "BYU Dictionaries"
  val expiration = Utils.getExpiration("byu")
  val codeFormat = 'iso639_3

  /**
   * Endpoint for translating via BYU Dictionaries
   */
  def translate(user: User, src: String, dst: String, text: String)
               (implicit restart: TRestart) = {
    val key = s"$src-$dst"
    DictionaryCache.getDictionaryEntry(key, text).orElse {
      DictionaryCache.getDictionaryEntry(key, text.toLowerCase)
    }.map { definition =>
      Json.obj(
        //"translations" -> Json.arr("free translation text")
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
                )
              )
            )
          )
        )
      )
    }
  }
}