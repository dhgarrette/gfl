package dhg.gfl

import org.junit.Assert._
import org.junit.Test

/**
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
class ReadmeTests {

  @Test
  def test() {

    /*
		 * Load a single sentence:
		 */

    import dhg.gfl.Fudg._

    val text = "The man walks a big dog ."
    val annotation = """
        (The man walks < (a dog*))  .
        big > dog
        (The man)
        """
    val sentenceOption: Option[Sentence] = fromGfl(text, annotation)
    val sentence = sentenceOption.get

    /*
     * Read annotation from files:
     */

    import dhg.util.CollectionUtil._
    import dhg.util.FileUtil._
    import scalaz._, Scalaz._

    import dhg.gfl.Fudg._
    val NonHiddenFile = "[^.].*".r

    val sentencesByLanguage =
      File("data/gfl").listFilesRecursive(NonHiddenFile).flatMap { f =>
        f.readLines.map(fromAnnotationJson)
      }.reduce(_ |+| _)

    val english = sentencesByLanguage("eng")
    val firstSentence = english.head

    /*
     * Getting sentence information:
     */

    sentence.tokens
    sentence.nodes
    sentence.edges

    /* 
     * Get all brackets inferred from this annotation.
     * A "bracket" is a pair (start,end) such that `sentence.tokens.slice(start,end)` 
     * is a bracketed unit.
     */
    val allBrackets: Vector[(Int, Int)] = sentence.brackets

    /* 
     * Get (and draw) the tree of `deps`.
     */
    val fudgTree = sentence.fudgTree
    dhg.util.viz.TreeViz.drawTree(fudgTree)

  }

}
