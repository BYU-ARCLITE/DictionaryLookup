package edu.byu.arclite.dictionary

import java.io.File
import java.text.Normalizer
import collection.concurrent.TrieMap
import collection.mutable.ListBuffer
import play.api.cache.Cache
import play.api.{Logger, Play}
import Play.{current, configuration}

/**
 * Created with IntelliJ IDEA.
 * User: camman3d
 * Date: 2/5/13
 * Time: 2:39 PM
 * To change this template use File | Settings | File Templates.
 */
object DictionaryCache {

  val expiration = configuration.getMilliseconds("dictionary.expiration")
    .map(_.toInt / 1000)
    .getOrElse(600) //default 10 minutes

  val basePath = configuration.getString("byu.dictpath").getOrElse("./dictionaries")

  def getDictionaryEntry(language: String, key: String): Option[List[String]] = try {
    val dictionary = Cache.getOrElse[TrieMap[String, ListBuffer[String]]](language, expiration) {
      val file = new File(basePath, language + ".bin")
      Logger.info(s"Loading dictionary $language from ${file.getPath()}")
      Dictionary.loadFromFile(file)
    }
    val normalizedKey = Normalizer.normalize(key, Normalizer.Form.NFC)
    dictionary.get(normalizedKey).orElse {
	  dictionary.get(normalizedKey.toLowerCase)
	}.flatMap { buf =>
      if(buf.length > 0) Some(buf.toList) else None
    }
  } catch {
    case e: Throwable =>
      Logger.info(s"Dictionary $language could not be loaded: ${e.getMessage()}")
      None
  }
}
