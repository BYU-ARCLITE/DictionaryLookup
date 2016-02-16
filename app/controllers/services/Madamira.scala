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
  val codeFormat = 'iso6239_3
  val madamiraURL = configuration.getString("services.madamira")

  def parseXml(text: String): List[String] = 
  {
    val XMLdoc = XML.loadString(text)
    val words = XMLdoc \\ "word_info"
    val svmp = words \\ "word" \\ "svm_prediction" 
    play.Logger.error(words.toString) 
    // we get multiple return information from Madamira, so to start I want to check and see which has the best score
    val ScoreList : Seq[String] = for {
     score <- words \\ "analysis" \\ "@score"
    } yield { score.toString }

    val max = ScoreList.max
    val index = ScoreList.indexOf(max)

    val lemma: Seq[String] = for { word <- words \\ "word"
    } yield { (word \\ "@word").toString}
    val res = lemma.toList
    play.Logger.debug("List of words "+res.toString)
    play.Logger.debug(res.length.toString)
    


    val relemma = lemma.toList

    val length: Seq[String] = for { word <- words \\ "word"
    } yield { (word \\ "@length").toString}
    val relength = length.toList

    val offset: Seq[String] = for { word <- words \\ "word"
    } yield { (word \\ "@offset").toString}
    val reoffset = offset.toList

    val typ: Seq[String] = for { word <- words \\ "word"
    } yield { (word \\ "@type").toString}
    val retype = typ.toList

    val diac: Seq[String] = for {
      morph <- words \\ "word" \\ "svm_prediction" \\ "morph_feature_set"
    } yield { (morph \\ "@diac").toString }
    val rediac = diac.toList
    
    val asp: Seq[String] = for { morph <- words \\ "word" \\ "svm_prediction" \\ "morph_feature_set"
    } yield { (morph \\ "@asp").toString }
    val reasp = asp.toList

    val cas: Seq[String] = for { morph <- words \\ "word" \\ "svm_prediction" \\ "morph_feature_set"
    } yield { (morph \\ "@cas").toString}
    val recas = cas.toList

    val gen: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@gen").toString }
    val regen = gen.toList

    val mod: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@mod").toString }
    val remod = mod.toList

    val num: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@num").toString }
    val renum = num.toList

    val per: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@per").toString }
    val reper = per.toList

    val pos: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@pos").toString }
    val repos = pos.toList

    val stt: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@stt").toString }
    val restt = stt.toList

    val vox: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@vox").toString }
    val revox = vox.toList

    val prc0: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@prc0").toString }
    val reprc0 = prc0.toList

    val prc1: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@prc1").toString }
    val reprc1 = prc1.toList

    val prc2: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@prc2").toString }
    val reprc2 = prc2.toList

    val prc3: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@prc3").toString }
    val reprc3 = prc3.toList

    val enc0: Seq[String] = for { morph <- svmp \\ "morph_feature_set" 
    } yield { (morph \\ "@prc3").toString }
    val reenc0 = enc0.toList

    play.Logger.debug("ENDING STUFF")
    play.Logger.debug("LEMMA "+relemma.toString)
    play.Logger.debug("LENGTH "+relength.toString)
    play.Logger.debug("OFFSET "+reoffset.toString)
    play.Logger.debug("TYPE "+retype.toString)
    play.Logger.debug("DIAC (taken from morph_feature_set)"+rediac.toString)
    play.Logger.debug("ASP (taken from morph_feature_set)"+reasp.toString)
    play.Logger.debug("CAS (taken from morph_feature_set)"+recas.toString)
    play.Logger.debug("GEN (taken from morph_feature_set)"+regen.toString)
    play.Logger.debug("MOD (taken from morph_feature_set)"+remod.toString)
    play.Logger.debug("NUM (taken from morph_feature_set)"+renum.toString)
    play.Logger.debug("PER (taken from morph_feature_set)"+reper.toString)
    play.Logger.debug("POS (taken from morph_feature_set)"+repos.toString)
    play.Logger.debug("STT (taken from morph_feature_set)"+restt.toString)
    play.Logger.debug("VOX (taken from morph_feature_set)"+revox.toString)
    play.Logger.debug("PRC0 (taken from morph_feature_set)"+reprc0.toString)
    play.Logger.debug("PRC1 (taken from morph_feature_set)"+reprc1.toString)
    play.Logger.debug("PRC2 (taken from morph_feature_set)"+reprc2.toString)
    play.Logger.debug("PRC3 (taken from morph_feature_set)"+reprc3.toString)
    play.Logger.debug("ENC0 (taken from morph_feature_set)"+enc0.toString)
    res
  }

  def translate(user: User, src: String, dest: String, text: String): Option[Seq[String]] = {
    play.Logger.debug("In the madamira lookup")
    val dialect = if (src == "arz") "EGY"
    else "MSA"

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