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

case class MAnalysis(start: Long, end: Long, lemma: String, tokens: Node, features: Node)

object LookupMadamira extends Translator {
  val name = "Madamira"
  val expiration = Utils.getExpiration("Madamira")
  val codeFormat = 'iso6239_3
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
        Seq(MAnalysis(start, end, lemma, tokens, features))
      } catch {
        case _: Throwable => Nil
      }
    }
  }

  def getDefinitions(lemmas: Seq[MAnalysis], restart: TRestart):
                    (Set[String], Seq[JsObject]) = {
    val exclusion = Set("Madamira")

    // We're ignoring free translations for now.

    val results = lemmas.flatMap { wdata: MAnalysis =>
      (for {
        (names, result) <- restart(wdata.lemma, exclusion)
        wlist <- (result \ "words").asOpt[Seq[JsObject]]
      } yield {
        //TODO: Add an additional word entry for the gloss &
        // morphological information that we got from Madamira
        // or integrate that into existing entries, and filter
        // out entries that don't match the proper POS

        //update the start/end indices to match the surrounding text
        val updated_words = wlist.map { wstruct =>
          wstruct ++ Json.obj("start" -> wdata.start, "end" -> wdata.end)
        }
        (names, updated_words)
      }).toList
    }

    val names = results.foldLeft(Set[String]()) {
      case (acc, (n:Set[String],_)) => acc ++ n
    }

    val words = results.foldLeft(Seq[JsObject]()) {
      case (acc, (_,w:JsObject)) => acc ++ w
    }

    (names, words)
  }

  def translate(user: User, src: String, dst: String, text: String)
               (implicit restart: TRestart) = {

    dialects.get(src).flatMap { dialect =>
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
        val lemmas = parseXml(result.body)
        val (names, words) = getDefinitions(lemmas, restart)
        if (words.size == 0) None
        else Some((names ++ Set("Madamira"), Json.obj("words" -> words)))
      }
    }
  }
}