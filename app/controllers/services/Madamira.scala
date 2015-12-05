package controllers

import models.{User, ServiceLog}
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global

import xml.{Elem, XML}
import java.io.InputStream;
import java.net.URL;

import scalaj.http.Http

object LookupMadamira extends Translator {
  val name = "Madamira"
  val expiration = Utils.getExpiration("Madamira")
  val codeFormat = 'iso639_3

  val madamiraURL = configuration.getString("services.madamira")

  val dialects = Map("ara" -> "MSA", "arz" -> "EGY")

  def parseXml(text: String): List[String] = {
    val XMLdoc = XML.loadString(text)
    val words = XMLdoc \\ "word_info"
    // we get multiple return information from Madamira, so to start I want to check and see which has the best score
    val ScoreList : Seq[String] = for {
     score <- words \\ "analysis" \\ "@score"
    } yield { score.toString }

    val max = ScoreList.max
    val index = ScoreList.indexOf(max)

    val lemma: Seq[String] = for {
      word <- words \\ "word"
    } yield {
      (word \\ "@word").toString
    }
    val res = lemma.toList
    play.Logger.debug("List of words "+res.toString)
    play.Logger.debug(res.length.toString)
    res
  }

  def translate(user: User, src: String, dest: String, text: String): Option[Seq[String]] = {
    play.Logger.debug("In the madamira lookup")
    val dialect = if (src == "arz") "EGY" else "MSA"

    val inputXmlData = raw"""<?xml version="1.0" encoding="UTF-8"?>
    <madamira_input xmlns="urn:edu.columbia.ccls.madamira.configuration:0.1">
      <madamira_configuration>
          <preprocessing sentence_ids="false" separate_punct="true" input_encoding="UTF8"/>
          <overall_vars output_encoding="UTF8" dialect="MSA" output_analyses="TOP" morph_backoff="ADD_ALL" analyze_only="false"/>
          <requested_output>
              <req_variable name="PREPROCESSED" value="true" />
              <req_variable name="STEM" value="true" />
              <req_variable name="OFFSET" value="true" />
              <req_variable name="TYPE" value="true" />
              <req_variable name="LEMMA" value="true" />
              <req_variable name="DIAC" value="true" />
              <req_variable name="LENGTH" value="true" />
              <req_variable name="ASP" value="true" />
              <req_variable name="CAS" value="true" />
              <req_variable name="ENC0" value="true" />
              <req_variable name="ENC1" value="true" />
              <req_variable name="ENC2" value="true" />
              <req_variable name="GEN" value="true" />
              <req_variable name="MOD" value="true" />
              <req_variable name="NUM" value="true" />
              <req_variable name="PER" value="true" />
              <req_variable name="POS" value="true" />
              <req_variable name="PRC0" value="true" />
              <req_variable name="PRC1" value="true" />
              <req_variable name="PRC2" value="true" />
              <req_variable name="PRC3" value="true" />
              <req_variable name="STT" value="true" />
              <req_variable name="VOX" value="true" />
          </requested_output>
          <tokenization>
              <scheme alias="D3" />
              <scheme alias="D3_BWFORM" />
          </tokenization>
      </madamira_configuration>
      <in_doc id="ExampleDocument">
          <in_seg id="SENT1">
             $text
          </in_seg>
      </in_doc>
    </madamira_input>"""

    val url = "http://127.0.0.1:8223"
    val result = Http(url).postData(inputXmlData).header("content-type", "application/xml").asString
    play.Logger.debug("result: "+result.body)
    if(result.code != 200) {
      None
    } else {
      parseXml(result.body)
      Some(List(result.body))
    }
  }
}
