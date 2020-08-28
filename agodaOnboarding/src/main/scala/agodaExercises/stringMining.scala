package agodaExercises
import scala.collection.Map
import scala.collection.mutable.ArrayBuffer

class stringMining(val mystr : String) {
  val words: Array[String] = mystr.split(" ")

  def longestWord():String = {
    words.foldRight(words(0))((a,b) => if (a.length > b.length) a else b )
  }

  def mostCommonWord(): String = {
    var cache = Map[String, Int]()
    val countMap = words.foldRight(cache)((a,b) => {
      if (cache contains a)
        cache = cache + (a -> (cache(a)+1))
      else
        cache = cache + (a -> 1)
      cache
    } )

    val maxTuple = countMap.maxBy(_._2)
    maxTuple._1
  }

  def mostCommonLetter(): Char = {
    var cache = Map[Char, Int]()
    var countMap = words.foldRight(cache)((a,b) => {
      var letters = a.toCharArray
      letters.foldRight(cache)((c,d) => {
        if (cache contains c)
          cache = cache + (c -> (cache(c)+1))
        else
          cache = cache + (c -> 1)
        cache
      })
    })
    val maxTuple: (Char, Int) = countMap.maxBy(_._2)
    maxTuple._1
  }

  def charToWordMap(): Map[Char, ArrayBuffer[String]] = {
    var cache: Map[Char, ArrayBuffer[String]] = Map()
    words.foreach(a => a.toLowerCase)

    val mapArray = words.foldRight(cache)((a,b) => {
      var charArr = a.toCharArray
      charArr.foldRight(cache)((c,d) =>{

        cache = cache + (c -> (cache.getOrElse(c, ArrayBuffer.empty[String]) += a))
        cache
      })
    })
    mapArray
  }
}
