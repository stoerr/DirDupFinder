package net.stoerr.dirdupfinder

import java.io.File
import java.nio.file.{Paths, Files}
import java.security.MessageDigest

import sun.misc.BASE64Encoder

/**
 * @author ${user.name}
 */
object DirDupFinder {

  /** Sorted iterator over all files of a directory; treats links etc as normal files. */
  def fileIterator(file : File) : Iterator[File] = {
    if (!file.isDirectory) return Iterator(file)
    var children : Array[File] = file.listFiles()
    return children.sortBy(_.getPath).toIterator.flatMap(fileIterator(_))
  }

  val sha1 = MessageDigest.getInstance("SHA-1")
  val b64 = new BASE64Encoder()

  def digest(file: File) : String = {
    sha1.reset()
    return b64.encode(sha1.digest(Files.readAllBytes(file.toPath)))
  }

  def main(args : Array[String]): Unit = {
    fileIterator(new File(".")) foreach {f => println(f + "\t" + digest(f))}
  }

}
