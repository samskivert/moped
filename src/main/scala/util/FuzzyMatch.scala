//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.util

import moped._

/** Handles fuzzy matching of strings. A fuzzy match means that each character of `glob` appears in
  * the matched string, in order, with zero or more intervening characters. For example: `pnts`
  * fuzzy matches `peanuts`. */
class FuzzyMatch (glob :String) {

  /** Returns the subset of `strs` which fuzzy match `glob`, in order of match quality. */
  def filter (strs :Iterable[String]) :Seq[String] = filterBy(strs)(identity)

  /** Returns the subset of `as` which fuzzy match `glob` after being converted to strings via
    * `fn`, in order of match quality. */
  def filterBy[A] (as :Iterable[A])(fn :A => String) :Seq[A] = {
    case class Score[A] (a :A, astr :String, score :Int) extends Comparable[Score[A]] {
      def compareTo (other :Score[A]) = {
        val r0 = Integer.compare(score, other.score)
        if (r0 != 0) r0
        else compare(astr, other.astr)
      }
    }
    val sb = Seq.newBuilder[Score[A]] // (as.sizeHint)
    val iter = as.iterator ; while (iter.hasNext) {
      val a = iter.next() ; val astr = fn(a) ; val ascore = score(astr)
      if (ascore > 0) sb += Score(a, astr, -ascore)
    }
    sb.result().sorted.map(_.a)
  }

  /** Returns a match score `> 0` if `glob` fuzzy matches `full`, `0` if it does not match. */
  def score (full :String) :Int = {
    val glen = glob.length ; val flen = full.length
    if (glen == 0) 1
    else if (glen > flen) 0
    else {
      var score = 0 ; var consec = 0
      var gg = 0 ; var lg = adjustCase(glob.charAt(gg))
      var ff = 0 ; while (gg < glen && ff < flen) {
        val lf = adjustCase(full.charAt(ff))
        if (lg == lf) {
          // make consecutive matches that start with the very first character score higher than
          // consecutive matches later in the string
          if (ff == 0) consec += 1
          consec += 1
          score += consec
          gg += 1
          if (gg < glen) lg = adjustCase(glob.charAt(gg))
        } else consec = 0
        ff += 1
      }
      if (gg == glen) score else 0
    }
    // fuzzyMatch(full) match {
    //   case (false, _) => 0
    //   case (true, score) => score
    // }
  }

  def levenshteinDistance (full: String): Int = {
    val m = glob.length
    val n = full.length
    val dp = Array.ofDim[Int](m + 1, n + 1)

    for (i <- 0 to m) dp(i)(0) = i
    for (j <- 0 to n) dp(0)(j) = j

    for (i <- 1 to m) {
      for (j <- 1 to n) {
        val substitutionCost = if (glob(i - 1) == full(j - 1)) 0 else 1
        dp(i)(j) = List(dp(i - 1)(j) + 1, dp(i)(j - 1) + 1, dp(i - 1)(j - 1) + substitutionCost).min
      }
    }

    dp(m)(n)
  }

  val SEQUENTIAL_BONUS = 15 // bonus for adjacent matches
  val SEPARATOR_BONUS = 30 // bonus if match occurs after a separator
  val CAMEL_BONUS = 30 // bonus if match is uppercase and prev is lower
  val FIRST_LETTER_BONUS = 15 // bonus if the first letter is matched

  val LEADING_LETTER_PENALTY = -5 // penalty applied for every letter in str before the first match
  val MAX_LEADING_LETTER_PENALTY = -15 // maximum penalty for leading letters
  val UNMATCHED_LETTER_PENALTY = -1

  /**
   * Does a fuzzy search to find pattern inside a string.
   * @param {*} pattern string        pattern to search for
   * @param {*} str     string        string which is being searched
   * @returns [boolean, number]       a boolean which tells if pattern was
   *                                  found or not and a search score
   */
  def fuzzyMatch (str :String) = fuzzyMatchRecursive(
    str,
    0 /* patternCurIndex */,
    0 /* strCurrIndex */,
    null,
    Array.fill(256)(-1),
    256 /* maxMatches */,
    0 /* nextMatch */,
    0 /* recursionCount */,
    10 /* recursionLimit */
  )

  def fuzzyMatchRecursive(
    str :String,
    opatternCurIndex :Int,
    ostrCurIndex :Int,
    srcMatches :Array[Int],
    matches :Array[Int],
    maxMatches :Int,
    onextMatch :Int,
    orecursionCount :Int,
    recursionLimit :Int
  ) :(Boolean, Int) = {
    var outScore = 0

    // Return if recursion limit is reached.
    val recursionCount = orecursionCount + 1
    if (recursionCount >= recursionLimit) return (false, outScore)

    var nextMatch = onextMatch
    var patternCurIndex = opatternCurIndex
    var strCurIndex = ostrCurIndex

    // Return if we reached ends of strings.
    if (patternCurIndex == glob.length || strCurIndex == str.length) return (false, outScore)

    // Recursion params
    var recursiveMatch = false
    var bestRecursiveMatches = Array.fill(256)(-1)
    var bestRecursiveScore = 0

    // Loop through pattern and str looking for a match.
    var firstMatch = true
    while (patternCurIndex < glob.length && strCurIndex < str.length) {
      // Match found.
      if (adjustCase(glob(patternCurIndex)) == adjustCase(str(strCurIndex))) {
        if (nextMatch >= maxMatches) return (false, outScore)

        if (firstMatch && srcMatches != null) {
          Array.copy(srcMatches, 0, matches, 0, matches.length)
          firstMatch = false
        }

        var recursiveMatches = Array.fill(256)(-1)
        val (matched, recursiveScore) = fuzzyMatchRecursive(
          str,
          patternCurIndex,
          strCurIndex + 1,
          matches,
          recursiveMatches,
          maxMatches,
          nextMatch,
          recursionCount,
          recursionLimit
        )

        if (matched) {
          // Pick best recursive score.
          if (!recursiveMatch || recursiveScore > bestRecursiveScore) {
            System.arraycopy(recursiveMatches, 0, bestRecursiveMatches, 0, bestRecursiveMatches.length)
            bestRecursiveScore = recursiveScore
          }
          recursiveMatch = true
        }

        nextMatch += 1
        matches(nextMatch) = strCurIndex
        patternCurIndex += 1
      }
      strCurIndex += 1
    }

    val matched = patternCurIndex == glob.length

    if (matched) {
      outScore = 100

      // Apply leading letter penalty
      var penalty = LEADING_LETTER_PENALTY * matches(0)
      penalty = if (penalty < MAX_LEADING_LETTER_PENALTY) MAX_LEADING_LETTER_PENALTY else penalty
      outScore += penalty

      //Apply unmatched penalty
      val unmatched = str.length - nextMatch
      outScore += UNMATCHED_LETTER_PENALTY * unmatched

      // Apply ordering bonuses
      var i = 0; while (i < nextMatch) {
        val currIdx = matches(i)

        if (i > 0) {
          val prevIdx = matches(i - 1)
          if (currIdx == prevIdx + 1) {
            outScore += SEQUENTIAL_BONUS
          }
        }

        // Check for bonuses based on neighbor character value.
        if (currIdx > 0) {
          // Camel case
          val neighbor = str(currIdx - 1)
          val curr = str(currIdx)
          if (neighbor != neighbor.toUpper && curr != curr.toLower) {
            outScore += CAMEL_BONUS
          }
          val isNeighbourSeparator = neighbor == '_' || neighbor == ' ' || neighbor == '.'
          if (isNeighbourSeparator) {
            outScore += SEPARATOR_BONUS
          }
        } else {
          // First letter
          outScore += FIRST_LETTER_BONUS
        }

        i += 1
      }

      // Return best result
      if (recursiveMatch && (!matched || bestRecursiveScore > outScore)) {
        // Recursive score is better than "this"
        System.arraycopy(bestRecursiveMatches, 0, matches, 0, matches.length)
        outScore = bestRecursiveScore
        return (true, outScore)
      } else if (matched) {
        // "this" score is better than recursive
        return (true, outScore)
      } else {
        return (false, outScore)
      }
    }
    return (false, outScore)
  }

  protected def compare (astr :String, bstr :String) :Int = astr.compareTo(bstr)

  protected def adjustCase (c :Char) :Char = c
}

/** A case-insensitive [[FuzzyMatch]]. */
class IFuzzyMatch (glob :String) extends FuzzyMatch(glob) {
  override def adjustCase (c :Char) = Character.toLowerCase(c)
}

object FuzzyMatch {

  /** Returns a fuzzy matcher on `glob`. If `glob` contains any upper case characters, the match
    * will be case sensitive, otherwise it will be case insensitive. */
  def create (glob :String) :FuzzyMatch = {
    // if the glob string is mixed case, do exact case fuzzy matching
    if (glob.exists(Character.isUpperCase)) new FuzzyMatch(glob)
    // otherwise do case insensitive matching
    else createI(glob)
  }

  /** Returns a case insensitive fuzzy matcher on `glob`. */
  def createI (glob :String) :FuzzyMatch = new IFuzzyMatch(glob)

  /** Alias for [[create]] for Scala clients. */
  def apply (glob :String) = create(glob)
}
