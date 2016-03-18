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
                     lemma: String, glosses: Seq[String])

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
  val codeFormat = 'iso639_3
  val endpoint =  "http://nihongo.monash.edu/cgi-bin/wwwjdic"

  /** Maps the languages to the dictionary letter **/
  val codeMap = Map("eng" -> "1",
                    "deu" -> "G",
                    "fra" -> "H",
                    "rus" -> "I",
                    "swe" -> "J",
                    "hun" -> "K",
                    "spa" -> "L",
                    "nld" -> "M",
                    "slv" -> "N",
                    "ita" -> "O")

  def getPairs = {
    val codes = codeMap.keySet
    codes.map((_,"jpn")) ++ codes.map(("jpn",_))
  }

  def getXML(query: Future[WSResponse]) : Future[xml.Elem] = {
    query.map { response =>
      if (response.status != 200) throw new Exception()
      else {
        val data = response.body
                   .replace("""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">""", "")
                   .replace("""<META http-equiv="Content-Type" content="text/html; charset=UTF-8">""", "")
                   .replace("""<br>""", "\n")
                   .replace("""<p>""", "")

        XML.loadString(data)
      }
    }
  }

  def getTokens(text: String) = {
    val query = WS.url(s"$endpoint?9ZIG$text").get()
    val result = getXML(query).map { XMLDoc =>
      val entries = (XMLDoc \\ "li").map(_.text)

      val indices = (XMLDoc \\ "FONT").map { node =>
        val word = node.text
        val start = text.indexOfSlice(word)
        (start, word)
      }

      //TODO: Extract unglossed segments

      for {
        ((start, word), gloss) <- (indices zip entries)
      } yield {
        val end = start+word.length
        JAnalysis(start, end, word, Seq(gloss))
      }
    }.recover { case _ => Nil }

    Await.result(result, Duration.Inf)
  }

  //TODO: rewrite to produce a lemma/entry object
  //TODO: Figure out how to use parser combinators here
  def parseEntries(XMLDoc: xml.Elem) : Seq[String] = {
    (XMLDoc \\ "pre")
      .flatMap(_.text.split("\n"))
      .flatMap(_.split("""[,\s\/]*\(\d+\)"""))
      .map(_.trim).distinct
      .filterNot(_ == "")
  }

  def getDefinitions(text: String, dictCode: String) : Future[Seq[String]] = {
    //TODO: filter results according to the parts of speech
    // from the contextual glosses
    val query = WS.url(s"$endpoint?${dictCode}ZUQ$text").get()
    getXML(query).map(parseEntries).recover{ case _ => Nil }
  }

  def getAllDefinitions(tokens: Seq[JAnalysis], dictCode: String) = {
    val seqf = Future.sequence {
      tokens.map { case JAnalysis(start, end, word, _) =>
        getDefinitions(word, dictCode).map { defins =>
          if(defins.size == 0) None
          else Some(JAnalysis(start, end, word, defins))
        }
      }
    }
    Await.result(seqf, Duration.Inf).collect {
      case Some(analysis) => analysis
    }
  }

  def fromJapanese(text: String, dictCode: String) = {
    val tokens = getTokens(text)

    val analyses = if(dictCode == "eng") tokens
    else getAllDefinitions(tokens, dictCode)

    val words = analyses.map { case JAnalysis(start, end, word, glosses) =>
      val lemma = Json.obj(
        "representations" -> Json.arr("Orthographic"),
        "lemmaForm" -> "lemma",
        "forms" -> Json.obj(
          "lemma" -> Json.obj(
            "Orthographic" -> Seq(word)
          )
        ),
        "senses" -> glosses.map { text =>
          Json.obj("definition" -> text)
        },
        "sources" -> Json.arr(
          Json.obj("name" -> name, "attribution" -> s"<i>$name</i>")
        )
      )

      Json.obj(
        "start" -> start,
        "end" -> end,
        "lemmas" -> Seq(lemma)
      )
    }

    if(words.size == 0) None
    else Some(words)
  }

  def toJapanese (text: String, dictCode: String) = {
    val defs = Await.result(getDefinitions(text, dictCode), Duration.Inf)
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
    case ("jpn", _) =>
      codeMap.get(dst).flatMap { dcode =>
        fromJapanese(text, dcode)
      }
    case (_, "jpn") =>
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