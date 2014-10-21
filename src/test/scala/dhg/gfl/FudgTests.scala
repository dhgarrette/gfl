package dhg.gfl

import dhg.util._
import org.junit.Assert._
import org.junit.Test
import scalaz._
import Scalaz._

/**
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
class FudgTests {

  @Test
  def test_fromGfl_1 {
    val sentence = Fudg.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks < (a dog*))  .
      big > dog
      (The man)
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 2), (0, 6), (3, 6)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_6 {
    val sentence = Fudg.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks a > [big dog])  .
      (The man)
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 2), (0, 6), (4, 6)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_5 {
    val sentence = Fudg.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks < (a dog*))  .
      (The man)
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 2)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_2 {
    val sentence = Fudg.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks < (a dog))  .
      big > dog
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 6), (3, 6)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_3 {
    val sentence = Fudg.fromGfl(
      "a b c d",
      """
      b > [c d]
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((2, 4)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_7 {
    val sentence = Fudg.fromGfl(
      "a b c",
      """
      a b c
      b > c
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector(), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_4 {
    val sentence = Fudg.fromGfl(
      "a b c",
      """
      a > b
      b > c
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 2)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_8 {
    val validation = Fudg.fromGfl(
      "a b c",
      """
      a > b
      b > c
      c > a
      """)
    assertTrue(validation.isSuccess)
    val sentence = validation.getOrElseThrow()
    assertFalse(Fudg.isSemanticallyValid(sentence.edges, false))
    val errors = Fudg.semanticTreeErrors(sentence.edges)
    assertTrue(errors.nonEmpty)
    assertTrue(errors.exists(_.startsWith("cycle found")))
  }

  @Test
  def test_fromGfl_9 {
    val validation = Fudg.fromGfl(
      "a b c d",
      """
      a > b
      b > c
      c > d
      d > b
      """)
    assertTrue(validation.isSuccess)
    val sentence = validation.getOrElseThrow()
    assertFalse(Fudg.isSemanticallyValid(sentence.edges, false))
    val errors = Fudg.semanticTreeErrors(sentence.edges)
    assertTrue(errors.nonEmpty)
    assertTrue(errors.exists(_.startsWith("cycle found")))
  }

  @Test
  def test_fromGfl_10 {
    val validation = Fudg.fromGfl(
      "a b c",
      """
      c > a
      c > b
      """)
    assertTrue(validation.isSuccess)
    val sentence = validation.getOrElseThrow()
    assertFalse(Fudg.isSemanticallyValid(sentence.edges, false))
    val errors = Fudg.semanticTreeErrors(sentence.edges)
    assertTrue(errors.nonEmpty)
    assertTrue(errors.exists(_.startsWith("node W(c) has multiple parents")))
  }

  @Test
  def test_fromGfl_11 {
    val sentence = Fudg.fromGfl(
      "a b c",
      """
      a > b
      c > b
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector(), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_12 {
    val sentence = Fudg.fromGfl(
      "a b c d",
      """
      a > b
      c > b
      b > d
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 3)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_13 {
    val sentence = Fudg.fromGfl(
      "a b c d e f",
      """
          b       <      e
      a > b < c      d > e < f
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((3, 6)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_14 {
    val sentence = Fudg.fromGfl(
      "a b c d e f g",
      """
          b   >   d   <   f
      a > b < c       e > f < g
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 3), (4, 7)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_15 {
    val sentence = Fudg.fromGfl(
      // (a b c) d ((e f g) h i)
      "a b c d e f g h i",
      """
          b   >   d   <   i
      a > b < c           i   <    g           i < h
                               e > g < f
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 3), (4, 7), (4, 9)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_16 {
    val sentence = Fudg.fromGfl(
      "a b c d e f g h i",
      """
          b   >   d   <   i
      a > b < c           i   <   (g h)
                               e > g < f
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 3), (4, 8), (4, 9)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_17 {
    val sentence = Fudg.fromGfl(
      // (a b c) d ((e f g) h i)
      "a b c d e f g h i j",
      """
          b   >   d   <   j
      a > b < c           j   <   (g        h       i)
                               e > g < f
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 3), (4, 9), (4, 10)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_18 {
    val sentence = Fudg.fromGfl(
      // (a b c) d ((e f g) h i)
      "a b c d e f g h i j",
      """
          b   >   d   <   j
      a > b < c           j   <   (g   >    h   <   i)
                               e > g < f
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 3), (4, 7), (4, 9), (4, 10)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_19 {
    val sentence = Fudg.fromGfl(
      // (a b c) d ((e f g) h i)
      "a b c d e f g h i j",
      """
          b   >   d   <   j
      a > b < c           j   <   (g   >    h       i)
                               e > g < f
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 3), (4, 9), (4, 10)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_20 {
    val sentence = Fudg.fromGfl(
      // (a b c) d ((e f g) h i)
      "a b c d e f g h i j",
      """
          b   >   d   <   j
      a > b < c           j   <   (g        h   <   i)
                               e > g < f
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 3), (4, 9), (4, 10)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_fromGfl_21 {
    val sentence = Fudg.fromGfl(
      // (a b c) d ((e f g) h i)
      "a b c d e f g h i j",
      """
          b   >   d   <   j
      a > b < c           j   <   (g       (h       i))
                               e > g < f
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(sentence.fudgTree)
    assertEquals(Vector((0, 3), (4, 9), (4, 10), (7, 9)), sentence.brackets.toVector.sorted)
  }

  @Test
  def test_isSemanticallyValid {
    val s1 = Fudg.fromGfl(
      "The man walks a big dog .",
      """
      (The man walks < (a dog))  .
      big > dog
      
      """).getOrElseThrow()
    //dhg.util.viz.TreeViz.drawTree(s1.fudgTree)
    assertTrue(Fudg.isSemanticallyValid(s1.edges))
    assertFalse(Fudg.isSemanticallyValid(s1.addEdge(2, 4).edges))
    assertFalse(Fudg.isSemanticallyValid(s1.addEdge(4, 2).edges))

    //    val s2 = s1.addEdge(Edge(s1.node(1), s1.node(0), None))
    //    dhg.util.viz.TreeViz.drawTree(s2.fudgTree)
    //    assertEquals(Vector((0, 6), (3,6)), s2.brackets.sorted)
  }

}
