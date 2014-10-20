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
import scala.annotation.tailrec

/**
 * This is a scala interface to the CMU GFL parser, originally written in python.
 *
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
object Fudg {

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
          case WordRe(_) => WordFudgNodeType
          case MweRe(_) => MweFudgNodeType
          case FeRe(_) => FeFudgNodeType
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

  /**
   * Check whether this set of edges is valid
   *
   * <code>
   *   def graph_semantics_check(parse):
   *   """Do checks on the final parse graph -- these are linguistic-level checks,
   *   not graph definition checks."""
   *   # Check tree constraint
   *   for n in parse.nodes:
   *     outbounds = [(h,c,l) for h,c,l in parse.node_edges if c==n and l is None]
   *     if len(outbounds) > 1:
   *       raise InvalidGraph("Violates tree constraint: node {} has {} outbound edges: {}".format(
   *         repr(n), len(outbounds), repr(outbounds)))
   * <code>
   */
  def isSemanticallyValid(edges: Vector[Edge], throwOnFalse: Boolean = false): Boolean = {
    val parents = edges.collect { case Edge(parent, child, None) => child -> parent }.groupByKey
    for ((child, parents) <- parents) { println(f"${child.name} -> {${parents.map(_.name)}}") }; println

    val sb = new StringBuilder

    val multipleParents = parents.filter(_._2.size > 1)
    multipleParents.foreach {
      case (child, parents) =>
        sb.append(f"        node $child has multiple parents: $parents\n")
    }

    val cycles = findCycle(edges.collect { case Edge(parent, child, _) => (parent, child) })
    cycles.foreach { cycle =>
      sb.append(f"        cycle found involving nodes: ${cycles.mkString(", ")}\n")
    }

    if (throwOnFalse)
      throw new RuntimeException(f"Tree constraint violations found:\n" + sb)
    sb.isEmpty
  }

  @tailrec final def findCycle(allLinks: Vector[(Node, Node)]): Vector[Set[Node]] = {
    println(allLinks.map{case (Node(n1,_,_), Node(n2,_,_)) => f"$n1 -> $n2"}.mkString(", "))
    allLinks match {
      case (a, b) +: otherLinks =>
        if (allLinks.contains(b -> a)) {
          Vector(Set(b, a))
        }
        else {
          val linkedToByB = otherLinks.collect { case (`b`, c) => c }
          findCycle(otherLinks ++ linkedToByB.map(a -> _))
        }
      case _ => Vector()
    }
  }

}
