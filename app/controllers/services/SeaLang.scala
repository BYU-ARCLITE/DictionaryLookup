package controllers

import models.User
import Utils._
import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.ws._
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

  val ReExpr = raw""".*?см\. также\s*(.*?)\s*""".r
  def processSense(sense: JsObject): Option[Either[JsObject, String]] =
    (sense \ "$t").asOpt[String]
      .orElse {
        // there may be a POS in here, too, but it's
        // not clear how to associate it with a lemma
        (sense \ "def" \ "$t").asOpt[String]
      }
      .map {
      case ReExpr(nword) => Right(nword)
      case definition : String =>
        var result = Json.obj("definition" -> definition)

        (sense \ "usage" \ "$t").asOpt[String].foreach { str =>
          result = result ++ Json.obj("notes" -> Json.arr(str))
        }

        (sense \ "example" \ "$t").asOpt[String].foreach { str =>
          result = result ++ Json.obj("examples" -> Json.arr(str))
        }

        Left(result)
      }

  def processSenses(entry: JsObject): (Seq[JsObject],Seq[String]) =
    try {
      val senses = (entry \ "sense").get match {
      case a:JsArray => a.as[Seq[JsObject]].map(processSense(_))
      case o:JsObject => Seq(processSense(o))
      case _ => Nil
      }

      val defins = senses.collect { case Some(Left(d)) => d }
      val redirs = senses.collect { case Some(Right(w)) => w }
      (defins, redirs)
    } catch {
      case _: Throwable => (Nil, Nil)
    }

  def processEntries(json: JsObject, restart: TRestart): Seq[JsObject] = {
    val entries = for {
      entry <- (json \ "return" \ "entry").as[Seq[JsObject]];
      word <- (entry \ "form" \ "orth" \ "$t").asOpt[String]
    } yield {

      play.api.Logger.debug("Word: " + word)
      val (defins, redirs) = processSenses(entry)

      val lOpt = if (defins.size > 0) {
        var lemma = Json.obj(
          "representations" -> Json.arr("Orthographic"),
          "lemmaForm" -> "lemma",
          "forms" -> Json.obj(
            "lemma" -> Json.obj(
              "Orthographic" -> Json.arr(word)
            )
          ),
          "senses" -> defins,
          "sources" -> Json.arr(
            Json.obj("name" -> name, "attribution" -> s"<i>$name</i>")
          )
        )

        (entry \\ "pos").lift(0)
          .flatMap { pos => (pos \ "$t").asOpt[String] }
          .foreach { pos => lemma = lemma ++ Json.obj("pos" -> pos) }

        Some(lemma)
      } else None

      (lOpt, redirs)
    }

    val lemmas = entries.collect { case (Some(lemma), _) => lemma }

    val nset = Set[String]()
    val nwords = entries
                  .collect { case (_, redirs) => redirs }.flatten
                  .flatMap { nword => restart(nword, nset).toList }
                  .flatMap { jword => (jword \\ "lemmas") }
                  .flatMap {
                    case JsArray(nl) =>
                      nl.flatMap(_.asOpt[JsObject].toList)
                    case _ => Nil
                  }

    lemmas ++ nwords
  }

  def processQuery(querystr: (String, String)*)
                  (implicit restart: TRestart): Option[Seq[JsObject]] = {
    val query = WS.url("http://www.sealang.net/russtest/api.pl")
      .withQueryString(querystr:_*).get()

    val response = Await.result(query, Duration.Inf)

    if(response.status != 200) None
    else response.json.asOpt[JsObject].flatMap { json =>
      play.api.Logger.debug("SeaLang: "+json.toString())
      val lemmas = processEntries(json, restart)
      if (lemmas.size > 0) Some(lemmas)
      else None
    }
  }

  /**
   * English to Russian Translations
   */
  def englishToRussian(text: String)
                      (implicit restart: TRestart) =
    processQuery("service" -> "dictionary", "query" -> text,
                 "format" -> "json", "phrase" -> text,
                 "number" -> "5", "fold" -> "yes",
                 "resource" -> "l2l1","encode" -> "unicode")

 /**
  * Russian To English Translations
  */
  def russianToEnglish(text: String)
                      (implicit restart: TRestart) =
    processQuery("service" -> "dictionary", "query" -> text,
                 "format" -> "json", "phrase" -> text,
                 "number" -> "5", "fold" -> "yes",
                 "resource" -> "l1l2","encode" -> "unicode")

  /**
   * Specifically Handles Russian Definitions
   */
  def russianToRussian(text: String)
                      (implicit restart: TRestart) =
    processQuery("service" -> "dictionary", "query" -> text,
                 "format" -> "json", "phrase" -> text,
                 "number" -> "5", "fold" -> "yes",
                 "resource" -> "l1l2","encode" -> "unicode")

  /**
   * Endpoint for translating via SeaLang
   */
  def translate(user: User, src: String, dst: String, text: String)
               (implicit request: RequestHeader, restart: TRestart) = {
    ((src, dst) match {
    case ("eng","rus") =>  // English to Russian Translation
      englishToRussian(text)
    case ("rus", "eng") => // Russian to English Translation
      russianToEnglish(text)
    case ("rus", "rus") => // Russian To Russian
      russianToRussian(text)
    case _ => None
    }).map { lemmas =>
      Json.obj(
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