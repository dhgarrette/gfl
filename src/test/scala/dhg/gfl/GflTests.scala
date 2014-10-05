package dhg.gfl

import org.junit.Assert._
import org.junit.Test

/**
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
class GflTests {

  @Test
  def test_fromGfl_1 {
    val sentence = Gfl.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks < (a dog*))  .
      big > dog
      (The man)
      """).get
    //dhg.util.viz.TreeViz.drawTree(sentence.depTree)
    assertEquals(Vector((0, 2), (0, 6), (2, 6), (3, 6), (4, 6)), sentence.brackets.sorted)
  }

  @Test
  def test_fromGfl_2 {
    val sentence = Gfl.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks < (a dog))  .
      big > dog
      """).get
    //dhg.util.viz.TreeViz.drawTree(sentence.depTree)
    assertEquals(Vector((0, 6), (2, 6), (3, 6), (4, 6)), sentence.brackets.sorted)
  }

  @Test
  def test_fromGfl_3 {
    val sentence = Gfl.fromGfl(
      "a b c",
      """
      b > c
      """).get
    //dhg.util.viz.TreeViz.drawTree(sentence.depTree)
    assertEquals(Vector(), sentence.brackets.sorted)
  }

  @Test
  def test_fromGfl_4 {
    val sentence = Gfl.fromGfl(
      "a b c",
      """
      a > b
      b > c
      """).get
    //dhg.util.viz.TreeViz.drawTree(sentence.depTree)
    assertEquals(Vector((0, 2)), sentence.brackets.sorted)
  }

}
