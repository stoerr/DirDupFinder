package net.stoerr.dirdupfinder

import java.io.FileInputStream
import java.nio.file.{Files, Path}
import java.security.MessageDigest

import sun.misc.BASE64Encoder

/**
 * Utilities for calculating FileInfos
 * @author <a href="http://www.stoerr.net/">Hans-Peter Stoerr</a>
 * @since 03.09.2014
 */
object FileInfo {

  private val sha1 = MessageDigest.getInstance("SHA-1")
  private val b64 = new BASE64Encoder()
  private val buf: Array[Byte] = Array.fill(1024 * 1024)(0)

  def digestString(string: String): String = {
    sha1.reset()
    b64.encode(sha1.digest(string.getBytes("UTF-8")))
  }

  def sizeAndDigest(path: Path): (Long, String) = (Files.size(path), digest(path))

  def digest(path: Path): String = {
    sha1.reset()
    // return b64.encode(sha1.digest(Files.readAllBytes(file.toPath)))
    val stream = new FileInputStream(path.toFile)
    try {
      var bytesRead: Int = stream.read(buf)
      while (bytesRead > 0) {
        sha1.update(buf, 0, bytesRead)
        bytesRead = stream.read(buf)
      }
      b64.encode(sha1.digest())
    } finally {
      stream.close()
    }
  }

}
