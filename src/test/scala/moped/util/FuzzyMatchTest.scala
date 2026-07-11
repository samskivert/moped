//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.util

import org.junit._
import org.junit.Assert._

class FuzzyMatchTest {

  @Test def testExactMatch () :Unit = {
    var fuzy = new IFuzzyMatch("fuzy");
    Seq(
      "FuzzyMatch: moped.util. [Class]",
      "FuzzyMatchTest: moped.util. [Class]",
      "IFuzzyMatch:moped.util. [Class]",
      "FileFuzzyMatch:moped.Completer.File# [Class]",
    ).foreach { m => println(m + ": " + fuzy.score(m)) }

    var wizard = new IFuzzyMatch("wizarddata");
    Seq(
      "GGFolks.Wizard.WizSyncData:WizSyncData.cs@10[File]",
      "GGFolks.WizardAvatarData:AvatarData.cs@404 [File]",
      "GGFolks.Wizard.WizardData:WizardData.cs@18 [File]",
      "GGFolks.Wizard.CreateWizardPopup:CreateWizardPopup.cs@16 [File]",
      "GGFolks.Wizard.WizardNotification:WizardObject.cs@133[File]",
      "GGFolks.Wizard.EditWizardPanel:EditWizardPanel.cs@9[File]"
    ).foreach { m => println(m + ": " + wizard.score(m) + " " +
                             wizard.levenshteinDistance(m) + " " + wizard.fuzzyMatch(m)) }

  }
}
