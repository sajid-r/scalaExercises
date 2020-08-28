package agodaExercises

trait IntSet {
  def incl(x: Int): IntSet
  def contains(x: Int): Boolean
  def union(that: IntSet): IntSet = that.filterAcc(tweet => true,this)
//  def intersect(x: IntSet): IntSet = new
  def isEmpty: Boolean
  def excl(x: Int): IntSet
  def filter(p: Int => Boolean): IntSet = filterAcc(p, new EmptySet())
  def filterAcc(p: Int => Boolean, acc: IntSet): IntSet
}
case class EmptySet() extends IntSet {
  def contains(x: Int): Boolean = false
  def incl(x: Int): IntSet = new NonEmptySet(x, new EmptySet, new EmptySet)
  def intersect(x: IntSet): IntSet = this
  def isEmpty: Boolean = true
  def excl(x:Int): EmptySet = this
  def filterAcc(p: Int => Boolean, acc: IntSet): IntSet = acc
}
case class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
  def contains(x: Int): Boolean =
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true

  def filterAcc(p: Int => Boolean, acc: IntSet): IntSet = {
    def maybeInclude(p: Int => Boolean, acc: IntSet): IntSet =
      if (p(elem)) acc.incl(elem) else acc

    maybeInclude(
      p,
      left.filterAcc(p,
        right.filterAcc(p, acc))
    )
  }

  def isEmpty: Boolean = false

  def excl(x: Int): IntSet = {
    if (x < elem) new NonEmptySet(elem, left.excl(x), right)
    else if (elem < x) new NonEmptySet(elem, left, right.excl(x))
    else left.union(right)
  }
  def incl(x: Int): IntSet =
    if (x < elem) new NonEmptySet(elem, left incl x, right)
    else if (x > elem) new NonEmptySet(elem, left, right incl x)
    else this

//  def union(that: IntSet): IntSet = {
////    var newSet = new EmptySet().incl(elem)
//    def recIncl(acc: IntSet, x: IntSet) = x match {
//      case NonEmptySet(e, l, r) => {
//        var acc2 = acc.incl(e)
//        var acc3 = recIncl(acc2, l)
//        var acc4 = recIncl(acc3, r)
//        acc4
//      } : IntSet
//      case EmptySet() => acc
//    }
//
//    var newSet = recIncl(this, that)
//
//    newSet
//
//  }
}
