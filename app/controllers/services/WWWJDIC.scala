package controllers

import models.User
import Utils._
import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.ws.{WS, WSResponse}
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global
import xml.{Elem, XML, NodeSeq}

case class JAnalysis(start: Long, end: Long,
                     lemma: String, gloss: JsObject)

object LookupWWWJDIC extends Translator {
  /**
   *  WWWJDIC Japanese Dictionary Server
   *      http://edrdg.org/wwwjdic/wwwjdicinf.html
   *
   *  Example Requests
   *      EN-JA Dictionary: http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?QMUQbuild
   *      JA-EN Dictionary: http://www.csse.monash.edu.au/~jwb/cgi-bin/wwwjdic.cgi?QMUQ%E6%8E%9B%E3%81%91%E6%B8%A1%E3%81%99
   *
   *  All dictionaries in this list are two way.
   *
   */

  val name = "WWWJDIC"
  val expiration = Utils.getExpiration("WWWJDIC")
  val codeFormat = 'iso639_1
  val endpoint =  "http://nihongo.monash.edu/cgi-bin/wwwjdic"

  /** Maps the languages to the dictionary letter **/
  val codeMap = Map("en" -> "1",
                    "de" -> "G",
                    "fr" -> "H",
                    "ru" -> "I",
                    "sv" -> "J",
                    "hu" -> "K",
                    "es" -> "L",
                    "nl" -> "M",
                    "sl" -> "N",
                    "it" -> "O")

  def getPairs = {
    val codes = codeMap.keySet
    codes.map((_,"ja")) ++ codes.map(("ja",_))	
  }

  def getXML(query: Future[WSResponse]) : Option[xml.Elem] = {
    val response = Await.result(query, Duration.Inf)
    if (response.status != 200) None
    else {
      val data = response.body
                 .replace("""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">""", "")
                 .replace("""<META http-equiv="Content-Type" content="text/html; charset=UTF-8">""", "")
                 .replace("""<p>""", "")
                 .replace("""<br>""", "<br/>")

      Some(XML.loadString(data))
    }
  }

  def getTokens(text: String) = {
    val query = WS.url(s"$endpoint?9ZIG$text").get()
    getXML(query).map { XMLDoc =>
      /*val glosses = (XMLDoc \\ "li").map { node =>
        val entry = node.text
        val word = ""
        val pos = ""

        val gloss = Json.obj(
          "start" -> start,
          "end" -> end,
          "lemmas" -> Json.arr(
            Json.obj(
              "representations" -> Json.arr("Orthographic"),
              "pos" -> pos,
              "lemmaForm" -> "lemma",
              "forms" -> Json.obj(
                "lemma" -> Json.obj("Orthographic" -> Seq(word))
              ),
              "senses" -> gloss.map { dt =>
                Json.obj("definition" -> dt)
              },
              "sources" -> Json.arr(
                Json.obj("name" -> name, "attribution" -> s"<i>$name</i>")
              )
            )
          )
        )
      }*/

      (XMLDoc \\ "FONT").map { node =>
        val word = node.text
        val start = text.indexOfSlice(word)
        val end = start + word.length

        val gloss = Json.obj(
          "start" -> start,
          "end" -> end,
          "lemmas" -> Json.arr()
        )

        JAnalysis(start, end, word, gloss)
      }
    }.getOrElse(Nil)
  }

  def parseDefins(XMLDoc: xml.Elem) = {
    var defins = (XMLDoc \\ "pre")
	  .flatMap(_.text.split("\n"))
      .flatMap(_.split("""[,\s\/]*\(\d+\)"""))
      .map(_.trim).distinct
      .filterNot(_ == "")
    if (defins.length == 0) None
    else Some(defins)  
  }

  /**
   *  Translate From Japanese for all of the languages in the list above.
   *  TODO: Add back in to-Japanese translations without segmentation
   */
  def getWordDefinitions(text: String, dictCode: String): Seq[JsObject] = {
    getTokens(text).map { token =>
	  val word = token.lemma
      val query = WS.url(s"$endpoint?${dictCode}ZUQ$word").get()
      for {
	    doc <- getXML(query)
		defins <- parseDefins(doc)
	  } yield {
        val lemma = Json.obj(
          "representations" -> Json.arr("Orthographic"),
          "lemmaForm" -> "lemma",
          "forms" -> Json.obj(
            "lemma" -> Json.obj(
              "Orthographic" -> Json.arr(word)
            )
          ),
          "senses" -> defins.map { text =>
            Json.obj("definition" -> text)
          },
          "sources" -> Json.arr(
            Json.obj("name" -> name, "attribution" -> s"<i>$name</i>")
          )
        )

        Json.obj(
          "start" -> token.start,
          "end" -> token.end,
          "lemmas" -> Json.arr(lemma)
        )
      }
    }.collect { case Some(lemma) => lemma }
  }

  def translate(user: User, src: String, dst: String, text: String)
               (implicit request: RequestHeader, restart: TRestart) = {
    if (src == "ja") {
      codeMap.get(dst).flatMap { dcode =>
        val words = getWordDefinitions(text, dcode)
        if(words.size == 0) None
        else Some(Json.obj("words" -> words))
      }
    } else None
  }
}