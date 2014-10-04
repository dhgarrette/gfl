GFL-Scala
===========

This is a scala interface to the CMU GFL parser, originally written in python.


## Including GFL-Scala as a dependency

In `build.sbt`:

    resolvers ++= Seq(
      "dhg releases repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/releases",
      "dhg snapshot repo" at "http://www.cs.utexas.edu/~dhg/maven-repository/snapshots"
    )
    
    libraryDependencies += "dhg" % "gfl-scala_2.11" % "0.0.1-SNAPSHOT"
        


## Setup Dependencies

    git clone git@github.com:brendano/gfl_syntax.git
    export PYTHONPATH=$PYTHONPATH:$(pwd)/gfl_syntax/scripts
    export PYTHONPATH=$PYTHONPATH:$(pwd)/gfl_syntax/parser
    
    git clone git@github.com:erikrose/parsimonious.git
    export PYTHONPATH=$PYTHONPATH:$(pwd)/parsimonious/


## Usage

Load a single sentence:

    import dhg.gfl.Gfl.fromGfl

    val sentence = fromGfl(
      "The man walks a big dog .",
      """
        (The man walks < (a dog*))  .
        big > dog
        (The man)
        """).get

Read annotation from files:

    import dhg.util.CollectionUtil._
    import dhg.util.FileUtil._
    import scalaz._, Scalaz._

    import dhg.gfl.Gfl.fromAnnotationJson
    val NonHiddenFile = "[^.].*".r
    
    val sentencesByLanguage = 
      File("data/gfl").listFilesRecursive(NonHiddenFile).flatMap { f =>
        f.readLines.map(fromAnnotationJson)
      }.reduce(_ |+| _)
      
    val english = sentencesByLanguage("eng")
    val firstSentence = english.head

Getting sentence information:

    sentence.tokens
    setence.nodes
    sentence.deps

    /* 
     * Get all brackets inferred from this annotation.
     * A "bracket" is a pair (start,end) such that `sentence.tokens.slice(start,end)` 
     * is a bracketed unit.
     */
    val allBrackets: Vector[(Int,Int)] = sentence.brackets

    /* 
     * Get (and draw) the tree of `deps`.
     */
    val depTree = sentence.depTree
    dhg.util.viz.TreeViz.drawTree(depTree)
