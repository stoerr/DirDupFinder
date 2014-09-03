package net.stoerr.dirdupfinder

import java.nio.file.{FileSystems, Files, Path}

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Finds exact duplicates of directories in a directory tree.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.09.2014
 */
object ExactDirDupFinder {

  val path2hash = new mutable.HashMap[Path, String]

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) sys.error("Please give some directories in which I should look for duplicate files as arguments.")
    val argumentPaths = args.sorted.map(f => FileSystems.getDefault.getPath(f))
    argumentPaths foreach processPath
    path2hash groupBy (_._2) filter (_._2.size > 1) mapValues (_.keys.toList.sorted) foreach { case (hash, paths) =>
      println(hash + " : ")
      paths foreach println
      println()
    }
  }

  /** If a file, returns sha1 of contents; if a directory, returns sha1 of string of filename + sha1 of children */
  def processPath(path: Path): String = {
    if (Files.isDirectory(path)) {
      val contents = new StringBuilder
      val children: Array[Path] = Files.newDirectoryStream(path).iterator().toArray
      children foreach { child =>
        val sha1 = processPath(child)
        contents ++= child.getFileName.toString ++= "\t" ++= sha1 ++= "\n"
      }
      val hash = FileInfo.digest(contents.toString())
      path2hash += path -> hash
      return hash
    } else {
      return FileInfo.digest(path)
    }
  }

}
