package agodaExercises

class RichSequence(s: Seq[Int]) {
  def median(): Int = {
    val sortedSeq = s.sorted
    if (sortedSeq.length%2 != 0 ) {
      return sortedSeq(((sortedSeq.length - 1) / 2))
    }
    else {
          val mid = ((sortedSeq.length / 2) - 1)
          return (sortedSeq(mid) + sortedSeq(mid+1)) / 2
        }
  }
}