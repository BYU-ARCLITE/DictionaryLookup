package edu.byu.arclite.dictionary

import play.api.cache.Cache
import play.api.Play.current
import collection.concurrent.TrieMap
import collection.mutable.ListBuffer
import play.api.{Logger, Play}
import java.io.File
import java.text.Normalizer

/**
 * Created with IntelliJ IDEA.
 * User: camman3d
 * Date: 2/5/13
 * Time: 2:39 PM
 * To change this template use File | Settings | File Templates.
 */
object DictionaryCache {

  val expiration = Play.configuration.getInt("dictionary.expiration").get

  def getDictionaryEntry(language: String, key: String): Option[List[String]] = {
    try {
      val dictionary = Cache.getOrElse[TrieMap[String, ListBuffer[String]]](language, expiration) {
        Logger.info("Loading dictionary " + language)
        Dictionary.loadFromFile(new File("./dictionaries/" + language + ".bin"))
      }
      val normalizedKey = Normalizer.normalize(key, Normalizer.Form.NFC)
      dictionary.get(normalizedKey).flatMap { buf =>
        if(buf.length > 0) Some(buf.toList) else None
      }
    } catch {
      case _: Throwable => //Should be more specific, but... eh.
        Logger.info("Dictionary " + language + " not found.")
        None
    }
  }
}
