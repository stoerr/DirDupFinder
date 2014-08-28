package net.stoerr.dirdupfinder

/**
 * @author ${user.name}
 */
object DirDupFinder {

  def foo(x : Array[String]) = x.foldLeft("")((a,b) => a + b)

  def main(args : Array[String]) {
    println( "Hello World!" )
    println("concat arguments = " + foo(args))
  }

}
