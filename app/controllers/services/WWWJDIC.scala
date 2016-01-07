package controllers

import models.User
import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global
import xml.{Elem, XML, NodeSeq}

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

  /** Maps the languages to the dictionary letter **/
  val dictionaries = Map("en" -> "1",
                         "de" -> "G",
                         "fr" -> "H",
                         "ru" -> "I",
                         "sv" -> "J",
                         "hu" -> "K",
                         "es" -> "L",
                         "nl" -> "M",
                         "sl" -> "N",
                         "it" -> "O")
  /**
   *  Translate To and From Japanese for all of the languages in the list above.
   *  Uses the xml.Elem and XML class
   *  Can't be loaded by the xml parser for 3 reasons:
   *    1. Needs a systemId. The first .replace() gets rid of the doctype tag to avoid this error
   *    2. There is no matching </META> tag. The second .replace() gets rid of the <META...>.
   *    3. There is a random <p> with no matching </p> in every response. The third .replace() gets rid of it.
   *  Query String:
   *    1: the en-ja dictionary (replace with Q for comprehensive dictionary that returns results from multiple dictionaries)
   *    Z: backdoor entry. Returns definitions as text inside <pre>...</pre> element. (replace with M for a fully formatted HTML response)
   *    U: results in UTF-8
   *    Q: get exact matches (replace with P = common matches, R = P && Q, E every match)
   *    TODO: Create a way to toggle between the amount of results returned
   */
  def getTranslations(text: String, dictCode: String) = {
    val query = WS.url("http://nihongo.monash.edu/cgi-bin/wwwjdic?" + dictCode + "ZUQ" + text).get()
    
    // Get Results, remove newlines for ease of use with re
    val response = Await.result(query, Duration.Inf)
    if (response.status != 200) None
    else {
      // Clean up the html to be read by xml
      val data = response.body.replace("""<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">""", "").replace(
        """<META http-equiv="Content-Type" content="text/html; charset=UTF-8">""", "").replace("""<p>""", "")
       
      val XMLdoc = XML.loadString(data)
      // drops 1 to compensate for the leading newline
      var raw = (XMLdoc \\ "pre").text.split("\n")
      // Gets the first meaning of a definition which ends with (2)
      val defins: Seq[String] = raw.map(x => x.split("""\,?\ ?\/?\(2\).*$""")(0)).filterNot(_.trim == "")
      if (defins.length == 0) None
      else Some(defins)
    }
  }

  def translate(user: User, src: String, dest: String, text: String) = {
    if (src == "ja") {
      dictionaries.get(dest).flatMap { dcode =>
        getTranslations(text, dcode)
      }
    } else if (dest == "ja") {
      dictionaries.get(src).flatMap { dcode =>
        getTranslations(text, dcode)
      }
    } else None
  }
}