package edu.byu.arclite.dictionary

/**
 * The abstract dictionary loader class. This is used in the Dictionary object when creating a dictionary. This provides
 * an abstract interface so we can import dictionaries from various places.
 * Date: 2/5/2013
 * @author Josh Monson
 */
abstract class DictionaryLoader {
  def getEntries: Iterable[(String, String)]
}
