//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.impl

/** Hackery to work around JavaFX, modules, SBT and other chickens desirous of sacrifice. */
object MopedLauncher {
  def main (args :Array[String]) = Moped.main(args)
}
