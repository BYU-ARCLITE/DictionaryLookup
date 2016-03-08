package controllers

import models.User
import Utils._
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global

import xml._
import java.io.InputStream;
import java.net.URL;

import scalaj.http.Http

case class MAnalysis(start: Long, end: Long,
                     lemma: String, gloss: JsObject,
                     tokens: Node, features: Node)

object LookupMadamira extends Translator {
  val name = "Madamira"
  val expiration = Utils.getExpiration("Madamira")
  val codeFormat = 'iso639_3
  val madamiraURL = configuration.getString("services.madamira")

  val dialects = Map(
    "ara" -> "MSA", // Arabic
    "aao" -> "EGY", // Algerian Saharan
    "abh" -> "MSA", // Tajiki
    "abv" -> "MSA", // Baharna
    "acm" -> "MSA", // Mesopotamian
    "acq" -> "MSA", // Ta'izzi-Adeni
    "acw" -> "MSA", // Hijazi
    "acx" -> "MSA", // Omani
    "acy" -> "MSA", // Cypriot
    "adf" -> "MSA", // Dhofari
    "aeb" -> "MSA", // Tunisian
    "aec" -> "MSA", // Saidi
    "afb" -> "MSA", // Gulf Arabic
    "ajp" -> "MSA", // South Levantine
    "ajt" -> "MSA", // Judeo-Tunisian
    "aju" -> "MSA", // Judeo-Moroccan
    "apc" -> "MSA", // North Levantine
    "apd" -> "MSA", // Sudanese
    "arb" -> "MSA", // Modern Standard
    "arq" -> "MSA", // Algerian
    "ars" -> "MSA", // Najdi
    "ary" -> "MSA", // Moroccan
    "arz" -> "EGY", // Egyptian
    "auz" -> "MSA", // Uzbeki
    "avl" -> "EGY", // Eastern Egyptian Bedawi
    "ayh" -> "MSA", // Hadrami
    "ayl" -> "MSA", // Libyan
    "ayn" -> "MSA", // Sanaani
    "ayp" -> "MSA", // North Mesopotamian
    "bbz" -> "MSA", // Babalia Creole
    "jrb" -> "MSA", // Judeo-Arabic
    "jye" -> "MSA", // Judeo-Yemeni
    "pga" -> "MSA", // Sudanese Creole
    "shu" -> "MSA", // Chadian
    "sqr" -> "MSA", // Siculo
    "ssh" -> "MSA", // Shihhi
    "xaa" -> "MSA", // Andalusian
    "yhd" -> "MSA", // Judeo-Iraqi
    "yud" -> "MSA"  // Judeo-Tripolitanian
  )

  def genInputXML(dialect: String, text: String) = {
    raw"""<?xml version="1.0" encoding="UTF-8"?>
    <madamira_input xmlns="urn:edu.columbia.ccls.madamira.configuration:0.1">
      <madamira_configuration>
        <preprocessing sentence_ids="false" separate_punct="true" input_encoding="UTF8"/>
        <overall_vars output_encoding="UTF8" dialect="$dialect" output_analyses="TOP" morph_backoff="ADD_ALL" analyze_only="false"/>
        <requested_output>
          <req_variable name="STEM" value="true" />
          <req_variable name="GLOSS" value="true" />
          <req_variable name="LEMMA" value="true" />
          <req_variable name="DIAC" value="true" />

          <req_variable name="POS" value="true" />

          <req_variable name="OFFSET" value="true" />
          <req_variable name="LENGTH" value="true" />

          <req_variable name="TYPE" value="true" />

          <req_variable name="ASP" value="true" />
          <req_variable name="CAS" value="true" />
          <req_variable name="GEN" value="true" />
          <req_variable name="MOD" value="true" />
          <req_variable name="NUM" value="true" />
          <req_variable name="PER" value="true" />
          <req_variable name="STT" value="true" />
          <req_variable name="VOX" value="true" />

          <req_variable name="ENC0" value="true" />
          <req_variable name="ENC1" value="true" />
          <req_variable name="ENC2" value="true" />

          <req_variable name="PRC0" value="true" />
          <req_variable name="PRC1" value="true" />
          <req_variable name="PRC2" value="true" />
          <req_variable name="PRC3" value="true" />
        </requested_output>
        <tokenization>
          <scheme alias="D3_BWFORM" />
        </tokenization>
      </madamira_configuration>
      <in_doc id="ExampleDocument">
        <in_seg id="SENT1">
          $text
        </in_seg>
      </in_doc>
    </madamira_input>"""
  }

  val morphs = Seq(
    "per", //Person
    "asp", //Aspect
    "vox", //Voice
    "mod", //Mood
    "gen", //Gender
    "num", //Number
    "stt", //State
    "cas"  //Case
  )

  def featuresToJson(features: Node, start: Long, end: Long) = {
    val derivation = morphs.map { attr =>
      features \@ attr match {
      case "" | "na" | "u" => None
      case value:String => Some(s"$attr:$value")
      }
    }.collect { case Some(str) => str }

    val form:String = derivation match {
    case Nil => "Unknown"
    case s:Seq[String] => s.mkString(" ")
    }

    val gloss = features \@ "gloss" match {
    case "" => Nil
    case "NO_ANALYSIS" => Nil
    case term:String => Seq(term)
    }

    Json.obj(
      "start" -> start,
      "end" -> end,
      "lemmas" -> Json.arr(
        Json.obj(
          "representations" -> Json.arr("Orthographic"),
          "pos" -> (features \@ "pos"),
          "lemmaForm" -> "lemma",
          "forms" -> Json.obj(
            "lemma" -> Json.obj("Orthographic" -> Seq(features \@ "stem")),
            form -> Json.obj("Orthographic" -> Seq(features \@ "diac"))
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
  }

  def parseXml(text: String): Seq[MAnalysis] = {
    val XMLdoc = XML.loadString(text)
    (XMLdoc \\ "word").flatMap { word =>
      if (word \@ "type" != "ARABIC") Nil
      else try {
        val start = (word \@ "offset").toLong
        val end = (word \@ "length").toLong + start

        val tokens = (word \\ "tokenized" \\ "tok")(0)

        // TODO: This might be backwards....
        val analyses = (word \\ "analysis")
                         .sortBy(a => (a \@ "score").toFloat)
        val topAnalysis = analyses(0)
        val features = (topAnalysis \\ "morph_feature_set")(0)

        //TODO: Experiment with different source texts
        // to figure out what attributes actually provide
        // the best dictionary forms for recursive lookups
        val lemma = features \@ "stem"
        val gloss = featuresToJson(features, start, end)
        Seq(MAnalysis(start, end, lemma, gloss, tokens, features))
      } catch {
        case _: Throwable => Nil
      }
    }
  }

  def getDefinitions(analyses: Seq[MAnalysis], restart: TRestart) = {
    val exclusion = Set("Madamira")

    analyses.flatMap { wdata: MAnalysis =>

      wdata.gloss :: (for {
        result <- restart(wdata.lemma, exclusion)
        wlist <- (result \ "words").asOpt[Seq[JsObject]]
      } yield {

        //update the start/end indices to match the surrounding text
        wlist.map { wstruct =>
          wstruct ++ Json.obj("start" -> wdata.start, "end" -> wdata.end)
        }.toList

      }).getOrElse(Nil)
    }
  }

  def translate(user: User, src: String, dst: String, text: String)
               (implicit restart: TRestart) = {

    if (dst != "eng") None
    else dialects.get(src).flatMap { dialect =>
      val inputXmlData = genInputXML(dialect, text)

      val url = "http://127.0.0.1:8223"
      val result = Http(url)
                    .postData(inputXmlData)
                    .header("content-type", "application/xml")
                    .asString

      play.Logger.debug("Madamira result:\n "+result.body)
      if(result.code != 200) {
        None
      } else {
        val analyses = parseXml(result.body)
        val words = getDefinitions(analyses, restart)

        if (words.size == 0) None
        else Some(Json.obj("words" -> words))
      }
    }
  }
}