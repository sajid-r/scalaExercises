package agodaExercises
import scala.collection.mutable.ArrayBuffer

trait genQueue[T] {
  def put(elem: T)
  def get() : T
}

class strArray extends genQueue[String] {
  private val buf = new ArrayBuffer[String]
  def get(): String = buf.remove(0)
  def put(x: String) { buf += x }
}

trait reversing extends genQueue[String]{
  abstract override def put(elem: String): Unit = super.put(elem.reverse)
}