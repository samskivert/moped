//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._

@Major(name="mini-yesno", tags=Array("mini"), desc="""
  A minibuffer mode that asks the user to respond 'y' or 'n' to a question.
""")
class MiniYesNoMode (
  env     :Env,
  miniui  :MiniUI,
  promise :Promise[Boolean],
  prompt  :String
) extends MinibufferMode(env, promise) {

  def ynprompt = s"$prompt (y or n)"
  miniui.setPrompt(ynprompt)

  override def selfInsertCommand (typed :String) = typed match {
    case "y" => promise.succeed(true)
    case "n" => promise.succeed(false)
    case _   => miniui.setPrompt(s"Please answer y or n. $ynprompt")
  }
}
