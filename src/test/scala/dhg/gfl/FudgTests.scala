package dhg.gfl

import org.junit.Assert._
import org.junit.Test

/**
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
class GflTests {

  @Test
  def test_fromGfl_1 {
    val sentence = Fudg.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks < (a dog*))  .
      big > dog
      (The man)
      """).get
    //dhg.util.viz.TreeViz.drawTree(sentence.gflTree)
    assertEquals(Vector((0, 2), (0, 6), (3, 6)), sentence.brackets.sorted)
  }

  @Test
  def test_fromGfl_6 {
    val sentence = Fudg.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks a > [big dog])  .
      (The man)
      """).get
    //dhg.util.viz.TreeViz.drawTree(sentence.gflTree)
    assertEquals(Vector((0, 2), (0, 6), (4, 6)), sentence.brackets.sorted)
  }

  @Test
  def test_fromGfl_5 {
    val sentence = Fudg.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks < (a dog*))  .
      (The man)
      """).get
    //dhg.util.viz.TreeViz.drawTree(sentence.gflTree)
    assertEquals(Vector((0, 2)), sentence.brackets.sorted)
  }

  @Test
  def test_fromGfl_2 {
    val sentence = Fudg.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks < (a dog))  .
      big > dog
      """).get
    //dhg.util.viz.TreeViz.drawTree(sentence.gflTree)
    assertEquals(Vector((0, 6), (3, 6)), sentence.brackets.sorted)
  }

  @Test
  def test_fromGfl_3 {
    val sentence = Fudg.fromGfl(
      "a b c",
      """
      b > c
      """).get
    //dhg.util.viz.TreeViz.drawTree(sentence.gflTree)
    assertEquals(Vector(), sentence.brackets.sorted)
  }

  @Test
  def test_fromGfl_4 {
    val sentence = Fudg.fromGfl(
      "a b c",
      """
      a > b
      b > c
      """).get
    //dhg.util.viz.TreeViz.drawTree(sentence.gflTree)
    assertEquals(Vector(), sentence.brackets.sorted)
  }

  @Test
  def test_isSemanticallyValid {
    val s1 = Fudg.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks < (a dog))  .
      big > dog
      
      """).get
    //dhg.util.viz.TreeViz.drawTree(s1.fudgTree)
    assertFalse(Fudg.isSemanticallyValid(s1.addEdge(2, 4).edges))
    assertFalse(Fudg.isSemanticallyValid(s1.addEdge(4, 2).edges))
    
    //    val s2 = s1.addEdge(Edge(s1.node(1), s1.node(0), None))
    //    dhg.util.viz.TreeViz.drawTree(s2.fudgTree)
    //    assertEquals(Vector((0, 6), (3,6)), s2.brackets.sorted)
  }

}
