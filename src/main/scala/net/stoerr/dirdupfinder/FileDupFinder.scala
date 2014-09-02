package net.stoerr.dirdupfinder

import java.nio.ByteBuffer
import java.nio.file.{FileSystems, Files, Path, StandardOpenOption}
import java.security.MessageDigest

import sun.misc.BASE64Encoder

import scala.collection.JavaConversions._

/**
 * Simple file duplicate finder: scans all directories given as arguments
 * for files that have the same content. It prints the duplicates in CSV-ish
 * format ordered by descending size.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 02.09.2014
 */
object FileDupFinder {

  val sha1 = MessageDigest.getInstance("SHA-1")
  val b64 = new BASE64Encoder()
  val buf = ByteBuffer.allocate(1024 * 1024)

  /** Sorted iterator over all files of a directory; treats links etc as normal files. */
  def fileIterator(path: Path): Iterator[Path] = {
    if (!Files.isDirectory(path)) return Iterator(path)
    val children = Files.newDirectoryStream(path).iterator().toArray
    children.sorted.toIterator.flatMap(fileIterator)
  }

  def digest(path: Path): String = {
    sha1.reset()
    // return b64.encode(sha1.digest(Files.readAllBytes(file.toPath)))
    val channel = Files.newByteChannel(path, StandardOpenOption.READ)
    buf.clear()
    var bytesRead: Int = channel.read(buf)
    while (bytesRead > 0) {
      buf.flip()
      sha1.update(buf)
      buf.clear()
      bytesRead = channel.read(buf)
    }
    channel.close()
    b64.encode(sha1.digest())
  }

  def printFileDuplicates(files: Iterator[Path]) = {
    val filesAndContents = files.map(f => (f, digest(f))).toList
    val groupedDuplicates = filesAndContents.groupBy(_._2).filter(_._2.size > 1).values
    groupedDuplicates.toArray.sortBy(p => -Files.size(p(0)._1)) foreach { paths =>
      paths foreach { case (path, md) =>
        println(md + " , " + Files.size(path) + " , " + path)
      }
      println()
    }
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) sys.error("Please give some directories in which I should look for duplicate files as arguments.")
    val files = args.sorted.map(f => fileIterator(FileSystems.getDefault.getPath(f))).toIterator.reduce(_ ++ _)
    printFileDuplicates(files)
  }

}
