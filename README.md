gfl
===========


## Setup Dependencies

    git clone git@github.com:brendano/gfl_syntax.git
    export PYTHONPATH=$PYTHONPATH:$(pwd)/gfl_syntax/scripts
    export PYTHONPATH=$PYTHONPATH:$(pwd)/gfl_syntax/parser
    
    git clone git@github.com:erikrose/parsimonious.git
    export PYTHONPATH=$PYTHONPATH:$(pwd)/parsimonious/


## Usage

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
    println(firstSentence.deps.head)

