package dhg.gfl

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.Pattern._
import dhg.util.Subprocess
import dhg.util.viz.VizTree
import scalaz._
import Scalaz._
import argonaut._
import Argonaut._
import dhg.util.viz.TreeViz

/**
 * A `Sentence` is the primary way of using Fudg information.
 *
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
case class Sentence(tokens: Vector[Token], nodes: Map[String, Node], edges: Vector[Edge]) {

  private val tokenNodes: Map[Token, Node] = nodes.values.collect { case node @ WordNode(name, token) => (token, node) }.toMap

  /**
   * The underspecified dependency trees representing this sentence.
   */
  lazy val fudgTree: FudgTree = {
    val children = edges.map { case Edge(p, c, l) => (p.name, c.name) }.groupByKey
    def makeTree(nodeName: String): FudgTree = FudgTree(nodes(nodeName), children.getOrElse(nodeName, Vector.empty).map(makeTree)) // .sortBy(_.tokens.map(_.index).min)
    val heads = (nodes.keySet -- children.values.flatten).toVector
    heads.map(makeTree) match {
      case Coll(tree) => tree
      case trees => FudgTree(FeNode("<ROOT>"), trees)
    }
  }

  /**
   * All brackets inferred from this annotation.
   *
   * A "bracket" is a pair (start,end) such that `sentence.tokens.slice(start,end)`
   * is a bracketed unit.
   *
   * A bracket is an Fudg Expression or Multi-Word Expression node that covers
   * a contiguous span of words.
   */
  lazy val brackets: Set[(Int, Int)] = {
    def literalBrackets(t: FudgTree): Set[(Int, Int)] = {
      val indicesCovered = t.node match {
        case FeNode(_) => Some(t.indicesCoveredRecursively)
        case MweNode(_, tokens) => Some(tokens.map(_.index).toSet)
        case _ => None
      }
      val bracket = indicesCovered.collect { case ic if indicesAreSpan(ic) => (ic.min, ic.max + 1) }
      t.children.flatMap(literalBrackets).toSet ++ bracket
    }

    def unambiguousDepTreeBrackets(t: FudgTree): Set[(Int, Int)] = t.node match {
      case _: FeNode => Set.empty
      case _ =>
        val ic = t.indicesCoveredRecursively
        val bracket = indicesAreSpan(ic).option((ic.min, ic.max + 1))
        t.children.flatMap(unambiguousDepTreeBrackets).toSet ++ bracket
    }

    literalBrackets(fudgTree) | unambiguousDepTreeBrackets(fudgTree)
  }

  private def indicesAreSpan(indicesCovered: Set[Int]) = {
    indicesCovered.size > 1 && indicesCovered.size < tokens.size && indicesCovered.max - indicesCovered.min == indicesCovered.size - 1
  }

  /**
   * Retrieve a token by its index.
   */
  def token(i: Int): Token = {
    val t = tokens(i)
    assert(t.index == i, f"token has index=${t.index}, not i=$i")
    t
  }

  /**
   * Retrieve a token-level node by its index.
   */
  def node(i: Int): Node = tokenNodes.getOrElse(token(i), sys.error(f"token ${token(i)} not found: tokenNodes=${tokenNodes.toVector.sortBy(_._1.index)}"))

  /**
   * Add an edge to the Fudg representation of this sentence and return a new
   * `Sentence` with the expanded edge set.
   */
  def addEdge(e: Edge): Sentence = this.copy(edges = edges :+ e)

  /**
   * Add an edge to the Fudg representation of this sentence and return a new
   * `Sentence` with the expanded edge set. representing a directed dependency
   * between the tokens at the two specified indices.
   */
  def addEdge(parentIndex: Int, childIndex: Int): Sentence = {
    addEdge(Edge(node(parentIndex), node(childIndex), None))
  }

}

case class FudgTree(node: Node, children: Vector[FudgTree]) extends VizTree {
  /**
   * The name of the node at the root of this (sub)tree.
   */
  def label = node.name

  /**
   * The set of indices of all tokens below this node.
   */
  lazy val indicesCoveredRecursively: Set[Int] = node.tokens.map(_.index).toSet ++ children.flatMap(_.indicesCoveredRecursively)
}

case class Token(token: String, index: Int)
trait Node { def name: String; def tokens: Vector[Token] }
case class WordNode(name: String, token: Token) extends Node { def tokens = Vector(token) }
case class MweNode(name: String, tokens: Vector[Token]) extends Node
case class FeNode(name: String) extends Node { def tokens = Vector.empty }
case class Edge(parent: Node, child: Node, label: Option[String])
