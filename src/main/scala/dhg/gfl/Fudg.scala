package dhg.gfl

import dhg.util._
import dhg.util.viz._
import scalaz._
import Scalaz._
import argonaut._
import Argonaut._
import scala.annotation.tailrec

/**
 * This is a scala interface to the CMU GFL parser, originally written in python.
 *
 * @author Dan Garrette (dhg@cs.utexas.edu)
 */
object Fudg {

  /**
   * Interpret the given GFL annotation for the given sentence as
   * a FUDG expression.
   *
   * @param tokens  The sentence as a sequence of individual tokens
   * @param gfl     The GFL annotation text block
   * @return        A Validation object containing either the FUDG sentence
   *                or the `GFLError` validation message returned from the
   *                underlying CMU library.
   */
  def fromGfl(tokens: Vector[String], gfl: String): Validation[String, Sentence] = {
    fromGfl(tokens.mkString(" "), gfl)
  }

  /**
   * Interpret the given GFL annotation for the given sentence as
   * a FUDG expression.
   *
   * @param tokens  The sentence as a string
   * @param gfl     The GFL annotation text block
   * @return        A Validation object containing either the FUDG sentence
   *                or the `GFLError` validation message returned from the
   *                underlying CMU library.
   */
  def fromGfl(sentence: String, gfl: String, checkSemantics: Boolean = false): Validation[String, Sentence] = {
    val input = f"${sentence.trim}\n${gfl.trim}"
    callBinary(input, checkSemantics).map { fudgJsonString =>
      val fudgJson = Parse.parseOption(fudgJsonString).get
      //println(fudgJson.spaces2)

      /*
       * Map(
       *   "An"        -> Token("An",        0),
       *   "example~1" -> Token("example~1", 1))
       */
      val tokens: Map[String, Token] = fudgJson.field("tokens").get.array.get.map(_.string.get).zipWithIndex.map { case (token, i) => token -> Token(token, i) }.toMap
      assert(tokens.size == sentence.trim.splitWhitespace.size, f"tokens.size=${tokens.size} != sentence.size=${sentence.trim.splitWhitespace.size}")

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
       * Tokens that are not included in any node
       */
      val coveredTokens = n2w.values.flatten.toSet
      val uncoveredTokenNodes = tokens.collect { case (name, tok) if !coveredTokens(tok) => name -> WordNode(name, tok) }

      /*
       * Map(
       *   "W(example~1)"    -> WordNode("W(example~1)", Token("example~1", 1)),
       *   "MW(still_at_it)" -> MweNode("MW(still_at_it)", Vector(Token("still", 2), Token("at", 3), Token("it", 4))),
       *   "FE3"             -> FeNode("FE3))
       */
      val WordRe = """W\((.+)\)""".r
      val MweRe = """MW\((.+)\)""".r
      val FeRe = """FE(\d+)""".r
      val nodes: Map[String, Node] = fudgJson.field("nodes").get.array.get.map(_.string.get).mapTo { nodeName =>
        nodeName match {
          case WordRe(_) =>
            WordNode(nodeName, n2w(nodeName).only)
          case MweRe(_) =>
            assert(n2w(nodeName).nonEmpty)
            MweNode(nodeName, n2w(nodeName))
          case FeRe(_) =>
            assert(!n2w.contains(nodeName))
            FeNode(nodeName)
        }
      }.toMap ++ uncoveredTokenNodes

      val edges: Vector[Edge] = fudgJson.field("node_edges").get.array.get.map(_.array.get).map {
        case List(parent, child, label) => Edge(nodes(parent.string.get), nodes(child.string.get), label.string.filter(_ != "null"))
      }.toVector

      Sentence(tokens.values.toVector.sortBy(_.index), nodes, edges)
    }
  }

  /**
   * Convert from annotations format to FUDG JSON, as specified in FUDG_JSON.md.
   *
   * Copied from gfl_syntax/scripts/make_json.py, but modified to read from
   * stdin instead of file.
   *
   * Input: First line is the input sentence (whitespace separated tokens).
   * Subsequent lines are the GFL annotations.
   * Output: FUDG JSON string
   *
   */
  private[this] def callBinary(input: String, checkSemantics: Boolean = false): Validation[String, String] = {
    val cmd = """
from __future__ import print_function

import sys,re,os
try:
    import ujson as json
except ImportError:
    import json

import view
#sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..', 'gflparser'))
import gfl_parser
import string

lines = [line.strip() for line in sys.stdin]
tokens = lines[0].split()
code = '\n'.join(lines[1:])
try:
    parse = gfl_parser.parse(tokens, code, check_semantics="""+{if (checkSemantics) "True" else "False"}+""")
    #view.desktop_open(view.draw(parse, 'x'))
    parseJ = parse.to_json()
    print(json.dumps(parseJ), sep='\t')
except gfl_parser.GFLError as e:
    print(str(e), file=sys.stderr)
    sys.exit(100)
		  """

    val (exitcode, fudgJsonString, error) = Subprocess.findBinary("python").args("-c", cmd).callAllReturns(input)
    exitcode match {
      case 0 => fudgJsonString.success[String]
      case 100 => error.trim.failure[String]
      case _ => sys.error(s"ERROR CALLING: python binary\nReturncode: $exitcode\n$error")
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

          sentence <- fromGfl(tokens, annotation).toOption
        } yield sentence).toVector
    }).toMap
  }
  
  def emptyFromTokens(words: Vector[String]) {
    val tokens = words.zipWithIndex.mapt ( (w,i) => Token(w,i) )
    val nodes = tokens.map { tok => val name = f"${tok.token}_${tok.index}"; name -> WordNode(name, tok) }.toMap
    Sentence(tokens, nodes, edges = Vector.empty[Edge])
  }

  /**
   * Check whether this set of edges is semantically valid.
   *
   * <code>
   *     gfl_syntax/gflparser/gfl_parser.py, line 349
   *
   *     def graph_semantics_check(parse):
   *         """Do checks on the final parse graph -- these are linguistic-level checks,
   *         not graph definition checks.
   *
   *         This doesn't attempt to do fancy reasoning about FEs.
   *         E.g., a < (b* c d) precludes a < c, but this conflict will not be caught here.
   *         """
   *         # Check tree constraint over each fragment
   *         roots = set()
   *         rootFor = {}
   *
   *         for n in parse.nodes:
   *             outbounds = [(h,c,l) for h,c,l in parse.node_edges if c==n and l not in {'Anaph','fe*','fe'}]
   *             if len(outbounds) > 1:
   *                 raise GFLError("Violates tree constraint: node {} has {} outbound edges: {}".format(
   *                                                             repr(n), len(outbounds), repr(outbounds)))
   *             elif len(outbounds)==0:
   *                 roots.add(n)
   *             elif n=='**':
   *                 raise GFLError("Special root symbol ** cannot be a dependent")
   *             else:
   *                 h, c, _ = outbounds[0]
   *                 r = rootFor.setdefault(h,h)
   *                 rootFor[c] = r
   *                 for k,v in rootFor.items():
   *                     if v==c:
   *                         rootFor[k] = r
   *         if not roots:
   *             raise GFLError("Violates tree constraint: no root")
   *         else:
   *             for k,v in rootFor.items():
   *                 if v not in roots:
   *                     raise GFLError("Violates tree constraint: no root for node {}".format(repr(k)))
   * <code>
   */
  def isSemanticallyValid(edges: Vector[Edge], throwOnFalse: Boolean = false): Boolean = {
    val errors = semanticTreeErrors(edges)
    if (errors.nonEmpty && throwOnFalse)
      throw new RuntimeException(f"Tree constraint violations found:\n" + errors.map("    " + _).mkString("\n"))
    errors.isEmpty
  }

  /**
   * Return a list of semantic tree errors.
   */
  def semanticTreeErrors(edges: Vector[Edge]): Vector[String] = {
    val b = collection.mutable.Buffer[String]()

    val parents = edges.collect { case Edge(parent, child, label) if !label.exists(Set("Anaph", "fe*", "fe")) => child -> parent }.distinct.groupByKey
    val multipleParents = parents.filter(_._2.size > 1)
    multipleParents.foreach {
      case (child, parents) =>
        b.append(f"node ${child.name} has multiple parents: ${parents.map(_.name)}\n")
    }

    val cycles = findCycle(edges.collect { case Edge(parent, child, _) => (parent, child) }.distinct)
    cycles.foreach { cycle =>
      b.append(f"cycle found involving nodes: ${cycles.map(_.map(_.name)).mkString(", ")}\n")
    }

    if (parents.exists(_._1.name == "**")) {
      b.append(f"Special root symbol ** cannot be a dependent")
    }

    b.toVector
  }

  @tailrec private def findCycle(allLinks: Vector[(Node, Node)]): Vector[Set[Node]] = {
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
