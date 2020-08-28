package agodaExercises

object tryretry{
  @annotation.tailrec
  def retry[T](n: Int, wait: Int = 60000)(fn: => T): T = {
    try {
      return fn
    } catch {
      case e if n > 1 => Thread.sleep(wait)
      case _ => throw new Exception("Really Odd Time")
    }
    retry(n - 1)(fn)
  }
}