package net.stoerr.dirdupfinder

import java.nio.ByteBuffer
import java.nio.file.{Files, Path, StandardOpenOption}
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
  private val buf = ByteBuffer.allocate(1024 * 1024)

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

  def digest(string: String): String = {
    sha1.reset()
    return b64.encode(sha1.digest(string.getBytes("UTF-8")))
  }

}
