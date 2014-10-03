package dhg.gfl

import dhg.util.CollectionUtil._
import dhg.util.FileUtil._
import dhg.util.Subprocess
import scalaz._, Scalaz._
import argonaut._, Argonaut._

/**
 * http://www.ark.cs.cmu.edu/FUDG/
 * https://github.com/brendano/gfl_syntax/blob/master/scripts/FUDG_JSON.md
 *
 */
object Gfl {

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
          input = f"${tokens.trim}\n${annotation.trim}"
          fudgJsonString = Subprocess.findBinary("python").args("parse_gfl.py").call(input)
          fudgJson <- Parse.parseOption(fudgJsonString)

        } yield {
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
          val nodes: Map[String, Node] = fudgJson.field("nodes").get.array.get.map(_.string.get).mapTo { nodeName => Node(nodeName, n2w.getOrElse(nodeName, Vector.empty)) }.toMap

          val deps: Vector[Dep] = fudgJson.field("deps").get.array.get.map(_.array.get).map { case List(parent, child, _) => Dep(nodes(parent.string.get), nodes(child.string.get)) }.toVector

          Sentence(tokens, nodes, deps)
        }).toVector
    }).toMap
  }

  def main(args: Array[String]): Unit = {

    val NonHiddenFile = "[^.].*".r

    val sentencesByLanguage =
      File("data/gfl").listFilesRecursive(NonHiddenFile).flatMap { f =>
        f.readLines.map(fromAnnotationJson)
      }.reduce(_ |+| _)

    val english = sentencesByLanguage("eng")
    val firstSentence = english.head
    println(firstSentence.deps.head)

  }

}

//

case class Token(token: String, index: Int)
case class Node(name: String, tokens: Vector[Token])
case class Dep(parent: Node, child: Node)
case class Sentence(tokens: Map[String, Token], nodes: Map[String, Node], deps: Vector[Dep])
