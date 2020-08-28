package agodaExercises
import scala.collection.mutable
import scala.collection.mutable.Set

class charList(strList: List[String]) {
  def getCharList(): List[Char]= {
    var charList = mutable.Set[Char]()
    strList.foreach((x)=>{
      val chars = x.toCharArray
      chars.foreach((y) => {
        if (!charList.contains(y)) charList.add(y)
      })
    })
    charList.toList
  }
}
