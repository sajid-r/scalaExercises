package agodaExercises

import scala.collection.mutable.ArrayBuffer

object mapMultiply extends App {
  def mapMultiple(x:List[Int], m: Map[Int, Int]): ArrayBuffer[Int] = {
    val result = ArrayBuffer[Int]()

    x.foreach((num:Int) => {
      if (m contains num) {
        result += (num * m(num))
      }
    } )
    result
  }
}
