package edu.byu.arclite.dictionary

import java.io._

/**
 * This provides a simple and straightforward way of reading and writing files to and from files.
 * Date: 2/5/2013
 * @author Josh Monson
 */
object ObjectIO {
  /**
   * This writes any serializable object to a file.
   * @param obj The object to write
   * @param file The file where the object will be written
   */
  def writeObjectToFile(obj: AnyRef, file: File) {
    val fileOutput = new FileOutputStream(file)
    val objectOutput = new ObjectOutputStream(fileOutput)
    objectOutput.writeObject(obj)
    objectOutput.close()
    fileOutput.close()
  }

  /**
   * This reads in an object from a file.
   * @param file The file from which the object will be read.
   * @tparam C The type of the object. Once read, it will be cast to this type.
   * @return The newly created object
   */
  def createObjectFromFile[C](file: File): C = {
    val fileInput = new FileInputStream(file)
    val objectInput = new ObjectInputStream(fileInput)
    objectInput.readObject().asInstanceOf[C]
  }
}
