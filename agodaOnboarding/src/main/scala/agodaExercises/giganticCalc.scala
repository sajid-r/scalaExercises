package agodaExercises

class giganticCalc(val classSeq: Vector[Int]) {
  lazy val x : Vector[Int] = classSeq.foldLeft(Vector[Int]())((acc: Vector[Int], item: Int) => {
      acc.appended(item*item)
  })
  lazy val y: Int = x.sum
  lazy val z: Double = math.sqrt(y)

  def doGiganticCalc(): (Vector[Int], Int, Double) = {
    (x,y,z)
  }
}
