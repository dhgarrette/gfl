package dhg.gfl

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.Subprocess
import dhg.util.viz.VizTree
import scalaz._
import Scalaz._
import argonaut._
import Argonaut._
import dhg.util.viz.TreeViz

/**
 * http://www.ark.cs.cmu.edu/FUDG/
 * https://github.com/brendano/gfl_syntax/blob/master/scripts/FUDG_JSON.md
 *
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
object Gfl {

  def fromGfl(tokens: Vector[String], gfl: String): Option[Sentence] = {
    fromGfl(tokens.mkString(" "), gfl)
  }

  def fromGfl(sentence: String, gfl: String): Option[Sentence] = {
    val input = f"${sentence.trim}\n${gfl.trim}"
    val fudgJsonString = Subprocess.findBinary("python").args("parse_gfl.py").call(input)
    for (fudgJson <- Parse.parseOption(fudgJsonString)) yield {
      //println(fudgJson.spaces2)

      /*
       * Map(
       *   "An"        -> Token("An",        0),
       *   "example~1" -> Token("example~1", 1))
       */
      val tokens: Map[String, Token] = fudgJson.field("tokens").get.array.get.map(_.string.get).zipWithIndex.map { case (token, i) => token -> Token(token, i) }.toMap

      /*
       * Map(
       *   "W(example~1)"    -> Vector(Token("example~1", 1)),
       *   "MW(still_at_it)" -> Vector(Token("still", 2), Token("at", 3), Token("it", 4)))
       */
      val n2w: Map[String, Vector[Token]] = {
        val n2wJson = fudgJson.field("n2w").get
        n2wJson.objectFields.get.mapTo { nodeName => n2wJson.field(nodeName).get.array.get.map(tok => tokens(tok.string.get)).toVector.sortBy(_.index) }.toMap
      }

      /*
       * Map(
       *   "W(example~1)"    -> Node("W(example~1)", Vector(Token("example~1", 1))),
       *   "MW(still_at_it)" -> Node("MW(still_at_it)", Vector(Token("still", 2), Token("at", 3), Token("it", 4))),
       *   "FE3"             -> Node("FE3))
       */
      val WordRe = """W\((.+)\)""".r
      val MweRe = """MW\((.+)\)""".r
      val FeRe = """FE(\d+)""".r
      val nodes: Map[String, Node] = fudgJson.field("nodes").get.array.get.map(_.string.get).mapTo { nodeName =>
        val typ = nodeName match {
          case WordRe(_) => WordGflNodeType
          case MweRe(_) => MweGflNodeType
          case FeRe(_) => FeGflNodeType
        }
        Node(nodeName, n2w.getOrElse(nodeName, Vector.empty), typ)
      }.toMap

      val edges: Vector[Edge] = fudgJson.field("node_edges").get.array.get.map(_.array.get).map {
        case List(parent, child, label) => Edge(nodes(parent.string.get), nodes(child.string.get), label.string.filter(_ != "null"))
      }.toVector

      Sentence(tokens.values.toVector.sortBy(_.index), nodes, edges)
    }
  }

  /**
   * @param jsonString  a gfl annotation json expression
   * @return  map from languages to annotated sentences
   */
  def fromAnnotationJson(jsonString: String): Map[String, Vector[Sentence]] = {
    (for {
      json <- Parse.parseOption(jsonString).toSeq
      language <- json.objectFieldsOrEmpty
      languageJson <- json.field(language)
    } yield {
      language ->
        (for {
          groupNumber <- languageJson.objectFieldsOrEmpty
          group <- languageJson.field(groupNumber).toSeq
          sentenceNumber <- group.objectFieldsOrEmpty
          sentence <- group.field(sentenceNumber).toSeq

          submitted <- sentence.field("submitted").flatMap(_.array).toSeq if submitted.nonEmpty // an annotation has been submitted

          annotation <- sentence.field("anno").flatMap(_.string).toSeq
          tokens <- sentence.field("sent").flatMap(_.string).toSeq

          //_ = println(f"$language -> $groupNumber -> $sentenceNumber -> $annotation")

          sentence <- fromGfl(tokens, annotation)
        } yield sentence).toVector
    }).toMap
  }

  def main(args: Array[String]): Unit = {

    //    val NonHiddenFile = "[^.].*".r
    //
    //    val sentencesByLanguage =
    //      File("data/gfl").listFilesRecursive(NonHiddenFile).flatMap { f =>
    //        f.readLines.map(fromAnnotationJson)
    //      }.reduce(_ |+| _)
    //
    //    val english = sentencesByLanguage("eng")
    //    val firstSentence = english.head
    //    println(firstSentence.deps.head)

    //    val sentence = fromGfl(
    //      "The man walks a big dog .",
    //      """
    //        (The man walks < (a dog*))  .
    //        big > dog
    //        """).get
    val sentence = fromGfl(
      "the~1 man~2 walks~3",
      """
        man~2 > walks~3
        the~1
        """).get
    sentence.brackets foreach println
    TreeViz.drawTree(sentence.gflTree)

  }

}

//

case class Token(token: String, index: Int)
case class Node(name: String, tokens: Vector[Token], typ: GflNodeType)
case class Edge(parent: Node, child: Node, label: Option[String])
case class Sentence(tokens: Vector[Token], nodes: Map[String, Node], edges: Vector[Edge]) {

  lazy val gflTrees: Vector[GflTree] = {
    val children = edges.map { case Edge(p, c, l) => (p.name, c.name) }.groupByKey
    def makeTree(nodeName: String): GflTree = GflTree(nodes(nodeName), children.getOrElse(nodeName, Vector.empty).map(makeTree)) // .sortBy(_.tokens.map(_.index).min)
    val heads = (nodes.keySet -- children.values.flatten).toVector
    heads.map(makeTree)
  }

  def gflTree: GflTree = gflTrees match {
    case Vector(t) => t
    case trees => GflTree(Node("<ROOT>", Vector(), FeGflNodeType), trees)
  }

  /**
   * Get all brackets inferred from this annotation.
   *
   * A "bracket" is a pair (start,end) such that `sentence.tokens.slice(start,end)`
   * is a bracketed unit.
   *
   * A bracket is an FE or MWE node that covers a contiguous span of words.
   */
  lazy val brackets: Vector[(Int, Int)] = {
    def treeBrackets(t: GflTree): Vector[(Int, Int)] = {
      val indicesCovered = t.node.typ match {
        case FeGflNodeType => Some(t.indicesCoveredRecursively)
        case MweGflNodeType => Some(t.node.tokens.map(_.index).toSet)
        case _ => None
      }
      val bracket = indicesCovered.collect { case ic if indicesAreSpan(ic) => (ic.min, ic.max + 1) }
      t.children.flatMap(treeBrackets) ++ bracket
    }
    gflTrees.flatMap(treeBrackets)
  }

  private def indicesAreSpan(indicesCovered: Set[Int]) = {
    indicesCovered.size > 1 && indicesCovered.size < tokens.size && indicesCovered.max - indicesCovered.min == indicesCovered.size - 1
  }

}

case class GflTree(node: Node, children: Vector[GflTree]) extends VizTree {
  def label = node.name
  lazy val indicesCoveredRecursively: Set[Int] = node.tokens.map(_.index).toSet ++ children.flatMap(_.indicesCoveredRecursively)
}

sealed trait GflNodeType
case object WordGflNodeType extends GflNodeType
case object MweGflNodeType extends GflNodeType
case object FeGflNodeType extends GflNodeType
