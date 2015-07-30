package net.stoerr.dirdupfinder

import java.io.{File, FileOutputStream, PrintStream}
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

  val logfile = new File("dups.log")
  require(!logfile.exists)
  val log = new PrintStream(new FileOutputStream(logfile))

  val ignorePattern = """target|\.classpath|\.project|\.settings|\.idea|\.hg|\.svn|\.git|.*~|.*.bak|\.checkstyle""".r.pattern

  def ignorePredicate(p: Path): Boolean = ignorePattern.matcher(p.getFileName.toString).matches()

  def write(msg: Any) = {
    println(msg)
    log.println(msg)
  }

  def main(args: Array[String]): Unit = try {
    if (args.isEmpty) sys.error("Please give some directories in which I should look for duplicate files as arguments.")
    val argumentPaths = args.sorted.map(f => FileSystems.getDefault.getPath(f))
    argumentPaths foreach processPath
    write("\n\n\nSTART DUPS\n\n")
    val groupedInfos: Map[ChildInfo, List[Path]] = path2hash groupBy (_._2) filter (_._2.size > 1) mapValues (_.keys.toList.sorted)
    val childrenRemoved = filterChildren(groupedInfos,
      (p: (ChildInfo, List[Path]), c: (ChildInfo, List[Path])) =>
        containsOnlyChildren(p._2, c._2, isPrefixOf))
    childrenRemoved.toArray.sortBy(i => (-i._1.size, i._1.digest)) foreach { case (hash, paths) =>
      write(hash + " : ")
      paths foreach write
      write("\n")
    }
  } finally {
    log.close()
  }

  /** If a file, returns sha1 of contents; if a directory, returns sha1 of string of filename + sha1 of children */
  def processPath(path: Path): ChildInfo = {
    var hasError = false
    var cumulativeSize = 0L
    if (Files.isDirectory(path)) {
      println("Processing " + path)
      val contents = new StringBuilder
      val children: Array[Path] = Files.newDirectoryStream(path).iterator().toArray
      val ignored = children filter ignorePredicate
      if (!ignored.isEmpty) println("Ignoring " + ignored.mkString(" "))
      children filterNot ignorePredicate foreach { child =>
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

  def isPrefixOf(parent: Path, child: Path) = child.startsWith(parent)

  def containsOnlyChildren[T](parents: List[T], children: List[T], parentChild: (T, T) => Boolean): Boolean = {
    children.forall(c => parents.exists(p => parentChild(p, c)))
  }

  def filterChildren[T](elems: TraversableOnce[T], parentChild: (T, T) => Boolean): List[T] = {
    def insertAndFilter(next: T, firsts: List[T]): List[T] =
      firsts.filterNot(parentChild(next, _)) ++
        List(next).filterNot(c => firsts.exists(parentChild(_, next)))
    elems.foldRight(List.empty[T])(insertAndFilter)
  }

  case class ChildInfo(error: Boolean, size: Long, digest: String) {
    override def toString = if (error) "ERROR" else size + "\t" + digest
  }

}
