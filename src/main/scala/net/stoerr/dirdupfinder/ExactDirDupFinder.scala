package net.stoerr.dirdupfinder

import java.io.PrintStream
import java.nio.file.{FileSystems, Files, Path}

import scala.collection.JavaConversions._
import scala.collection.mutable

/**
 * Finds exact duplicates of directories in a directory tree.
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.09.2014
 */
object ExactDirDupFinder {

  val path2hash = new mutable.HashMap[Path, ChildInfo]

  val logfile = new PrintStream("target/dups.log")

  def write(msg: Any) = {
    println(msg)
    logfile.println(msg)
  }

  def main(args: Array[String]): Unit = {
    if (args.isEmpty) sys.error("Please give some directories in which I should look for duplicate files as arguments.")
    val argumentPaths = args.sorted.map(f => FileSystems.getDefault.getPath(f))
    argumentPaths foreach processPath
    write("\n\n\n")
    val groupedInfos = path2hash groupBy (_._2) filter (_._2.size > 1) mapValues (_.keys.toList.sorted)
    groupedInfos.toArray.sortBy(i => (-i._1.size, i._1.digest)) foreach { case (hash, paths) =>
      write(hash + " : ")
      paths foreach write
      write("\n")
    }
  }

  /** If a file, returns sha1 of contents; if a directory, returns sha1 of string of filename + sha1 of children */
  def processPath(path: Path): ChildInfo = {
    var hasError = false
    var cumulativeSize = 0L
    if (Files.isDirectory(path)) {
      val contents = new StringBuilder
      val children: Array[Path] = Files.newDirectoryStream(path).iterator().toArray
      children foreach { child =>
        try {
          val childInfo = processPath(child)
          if (!childInfo.error) {
            contents ++= child.getFileName.toString ++= "\t" ++= childInfo.size.toString ++= "\t" + childInfo.digest ++= "\n"
            cumulativeSize = cumulativeSize + childInfo.size
          }
          else hasError = true
        } catch {
          case e: Exception => write(child + " : " + e)
            hasError = true
        }
      }
      val hash = FileInfo.digestString(contents.toString())
      val info = ChildInfo(hasError, cumulativeSize, hash)
      if (!hasError) path2hash += path -> info
      return info
    } else {
      val fileInfo = FileInfo.sizeAndDigest(path)
      return ChildInfo(false, fileInfo._1, fileInfo._2)
    }
  }

  case class ChildInfo(error: Boolean, size: Long, digest: String) {
    override def toString = if (error) "ERROR" else size + "\t" + digest
  }

}
