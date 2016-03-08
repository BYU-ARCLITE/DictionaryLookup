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
                     lemma: String, glosses: Seq[JsObject])

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
                 .replace("""<br>""", "\n")
                 .replace("""<p>""", "")

      Some(XML.loadString(data))
    }
  }

  //TODO: Figure out how to use parser combinators here
  def parseGlossEntry(text: String) : Seq[JsObject] = {
    val body = text.split("\n").last.trim
    Seq[JsObject]()
    /*Json.obj(
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
    )*/
  }

  def getTokens(text: String) = {
    val query = WS.url(s"$endpoint?9ZIG$text").get()
    getXML(query).map { XMLDoc =>
      val entries = (XMLDoc \\ "li").map { node =>
        parseGlossEntry(node.text)
      }

      val indices = (XMLDoc \\ "FONT").map { node =>
        val word = node.text
        val start = text.indexOfSlice(word)
        (start, word)
      }

      //TODO: Restart on unglossed segments

      for {
        ((start, word), glosses) <- (indices zip entries)
      } yield {
        val end = start+word.length
        JAnalysis(start, end, word, glosses)
      }
    }.getOrElse(Nil)
  }

  //TODO: rewrite to produce a lemma/entry object
  //TODO: Figure out how to use parser combinators here
  def parseEntries(XMLDoc: xml.Elem) : Seq[JsObject] = {
    /*var defins = (XMLDoc \\ "pre")
      .flatMap(_.text.split("\n"))
      .flatMap(_.split("""[,\s\/]*\(\d+\)"""))
      .map(_.trim).distinct
      .filterNot(_ == "")
    if (defins.length == 0) None
    else Some(defins)*/
    Seq[JsObject]()
  }

  def getDefinitions(text: String, dictCode: String) : Seq[JsObject] = {
    //TODO: filter results according to the parts of speech
    // from the contextual glosses
    val query = WS.url(s"$endpoint?${dictCode}ZUQ$text").get()
    getXML(query).map(parseEntries).getOrElse(Nil)
  }

  def fromJapanese(text: String, dictCode: String) = {
    val words = getTokens(text).map { token =>
      val lemmas = token.glosses ++ getDefinitions(token.lemma, dictCode)
      Json.obj(
        "start" -> token.start,
        "end" -> token.end,
        "lemmas" -> lemmas
      )
    }
    if(words.size == 0) None
    else Some(words)
  }

  def toJapanese (text: String, dictCode: String) = {
    val defs = getDefinitions(text, dictCode)
    if (defs.size == 0) None
    else {
      val word = Json.obj(
        "start" -> 0,
        "end" -> text.length,
        "lemmas" -> defs
      )
      Some(Seq(word))
    }
  }

  //TODO: Automatically transliterate hiragana into romaji
  def translate(user: User, src: String, dst: String, text: String)
               (implicit request: RequestHeader, restart: TRestart) = {
    val result = (src, dst) match {
    case ("ja", _) =>
      codeMap.get(dst).flatMap { dcode =>
        fromJapanese(text, dcode)
      }
    case (_, "ja") =>
      codeMap.get(src).flatMap { dcode =>
        toJapanese(text, dcode)
      }
    case _ => None
    }

    result.map { words =>
      Json.obj("words" -> words)
    }
  }
}