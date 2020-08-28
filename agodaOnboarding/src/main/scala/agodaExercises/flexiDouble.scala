package agodaExercises

class flexiDouble(value: Double) {
  val dbval: Double = value
  def ~=(that : flexiDouble)(implicit diff: Double = 0.001): Boolean = {
    if ((this.dbval - that.dbval).abs <= diff) true
    else false
  }
}
