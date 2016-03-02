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
  }

  def parseXml(text: String) = {
    val XMLdoc = XML.loadString(text)
    (XMLdoc \\ "word_info").map { wordinfo =>
    
      // we get multiple return information from Madamira,
	  // so to start I want to check and see which has the best score
      val ScoreList : Seq[String] = for {
       score <- wordinfo \\ "analysis" \\ "@score"
      } yield { score.toString }
	
      val max = ScoreList.max
      val index = ScoreList.indexOf(max)
	  val wordList = (wordinfo \\ "word")
	  wordList(index)
	}
  }

  def getDefinitions(len: Long, lemmas: NodeSeq, restart: TRestart) = {
      val words: Seq[String] = for {
        word <- lemmas \\ "word"
      } yield {
        (word \\ "@word").toString
      }

	
    val exclusion = Set("Madamira")

	//TODO: Make requests for all segmented words
	val text = words(0)
    val result = restart(text, exclusion)

    //play.Logger.info(result)

    val res = words.toSet
    play.Logger.debug("List of words "+res.toString)
    //play.Logger.debug(res.length.toString)

	//TODO: Merge result data from the restart
    val definition = "Santi"
    
    Json.obj(
      //"translations" -> Json.arr("free translation text")
      "words" -> Json.arr(
        Json.obj(
          "start" -> 0,
          "end" -> len,
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
  
  def translate(user: User, src: String, dst: String, text: String)
               (implicit restart: TRestart) = {

    dialects.get(src).flatMap { dialect =>
      val inputXmlData = genInputXML(dialect, text)

      val url = "http://127.0.0.1:8223"
      val result = Http(url)
	                .postData(inputXmlData)
	                .header("content-type", "application/xml")
					.asString

      //play.Logger.debug("result: "+result.body)
      if(result.code != 200) {
        None
      } else {
        val lemmas = parseXml(result.body)
		val words = getDefinitions(text.length, lemmas, restart)
        Some((Set("Madamira"), words))
      }
	}
  }
}