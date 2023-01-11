//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.grammar

import java.io.File
import java.nio.file.Paths

object Convert {

  def main (args :Array[String]) :Unit = try {
    if (args.isEmpty) {
      println("Usage: moped.grammar.Convert file.tmLanguage");
      sys.exit(1)
    }
    args(0).substring(args(0).lastIndexOf(".")+1) match {
      case "json" => JsonGrammar.parse(Paths.get(args(0))).print(System.out)
      case "plist"|"tmLanguage" => PlistGrammar.parse(new File(args(0))).print(System.out)
      case suff => println(s"Unknown file suffix '$suff'")
    }
  } catch {
    case e :Throwable => e.printStackTrace(System.err)
  }
}
