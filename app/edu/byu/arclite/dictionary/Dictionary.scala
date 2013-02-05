package edu.byu.arclite.dictionary

import collection.concurrent.TrieMap
import collection.mutable.ListBuffer
import java.io._

/**
 * This object provides methods for creating and saving dictionaries.
 * Date: 2/5/2013
 * @author Josh Monson
 */
object Dictionary {

  /**
   * Given the name of kind of dictionary to load, this reads in the text file, creates the TrieMap, and saves it to a
   * binary file.
   * @param dictionaryLoader The dictionary loader
   */
  def createDictionary(dictionaryLoader: DictionaryLoader): TrieMap[String, ListBuffer[String]] = {
    // Create the Trie
    val dictionary = new TrieMap[String, ListBuffer[String]]()

    // Load the dictionary
    for (entry <- dictionaryLoader.getEntries) {
      if (dictionary contains entry._1)
        dictionary(entry._1) append entry._2
      else
        dictionary += entry._1 -> ListBuffer(entry._2)
    }

    // Return the dictionary
    dictionary
  }

  /**
   * This loads a dictionary file into a TrieMap structure and returns it
   * @param file The file containing the dictionary file structure
   * @return The dictionary
   */
  def loadFromFile(file: File): TrieMap[String, ListBuffer[String]] =
    ObjectIO.createObjectFromFile[TrieMap[String, ListBuffer[String]]](file)


  /**
   * This uses the ObjectIO object to write the dictionary to a file
   * @param dictionary The dictionary to save
   * @param file The file the dictionary will be written to
   */
  def saveToFile(dictionary: TrieMap[String, ListBuffer[String]], file: File) {
    ObjectIO.writeObjectToFile(dictionary, file)
  }

}
