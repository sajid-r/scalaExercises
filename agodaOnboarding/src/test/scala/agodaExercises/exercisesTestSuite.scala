package agodaExercises
import java.time.format.DateTimeFormatter

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.mutable.ArrayBuffer

class exercisesTestSuite extends AnyFunSuite {

  test("1. Testing getters and setters") {
    val myarray = new strArray()

    myarray.put("Hello")
    myarray.put("World")
    assert(myarray.get() == "Hello")
    assert(myarray.get() == "World")
  }

  test( testName = "2. Testing reverse using stackable trait") {
    val myarray = new strArray with reversing

    myarray.put("Hello")
    myarray.put("World")
    assert(myarray.get() == "olleH")
    assert(myarray.get() == "dlroW")
  }

  test( testName = "3. Testing implicit parameters") {
    val x0 : flexiDouble = new flexiDouble(123456789.123456789)
    val y0 : flexiDouble = new flexiDouble(123456789.123456788)

    implicit val someDiff: Double = 0.00000001
    assert((x0 ~= y0))

    val x1 : flexiDouble = new flexiDouble(123456789.123456789)
    val y1 : flexiDouble = new flexiDouble(123456789.123456769)

    assert(!(x1 ~= y1))

  }

  test(testName = "4. GCD using pattern matching") {
    var x = 12
    var y = 18
    assert(gcd.gcd(x, y) == 6)

    x = 12
    y = 12
    assert(gcd.gcd(x, y) == 12)

    x = 12
    y = 11
    assert(gcd.gcd(x, y) == 1)
  }

  test(testName = "5. timeit") {

    def timeit() : Unit = {
      val t1 = System.nanoTime

      var x = 12
      var y = 18
      println(gcd.gcd(x, y))

      val duration = (System.nanoTime - t1) / 1e9d
      println(duration)
    }

  }

  test(testName = "6. String Mining") {
    val mystr = new stringMining("Hello World. We are globetrotters. Goodbye World.")
    assert(mystr.longestWord() == "globetrotters.")
    assert(mystr.mostCommonWord() == "World.")
    assert(mystr.mostCommonLetter() == 'o')
    print(mystr.charToWordMap())
  }

  test(testName = "7. Char List for String List"){
    val strList = new charList(List[String]("Hello World", "This is Agoda", "Agoda is fun"))
    assert(strList.getCharList() == List[Char](' ', 'A', 'a', 'd', 'e', 'f', 'g', 'H', 'h', 'i', 'l', 'n', 'o', 'r', 's', 'T', 'u', 'W'))

    val emptyStrList = new charList(List[String]())
    assert(emptyStrList.getCharList() == List[Char]())
  }

  test(testName = "8. IntSet") {
    val intset1 = new NonEmptySet(elem = 3,
                                  left = new NonEmptySet(2, new EmptySet(), new EmptySet()),
                                  right = new NonEmptySet(elem = 5, new EmptySet, new NonEmptySet(elem = 7, new EmptySet, new EmptySet())))
    val intset2 = new NonEmptySet(elem = 4,
                                  left = EmptySet(),
                                  right = new EmptySet())
    assert(
      (intset1 union intset2) match {
        case NonEmptySet(elem, left, right) => true
        case _ => false
      }
    )
  }

  test(testName = "10. Squared List") {
    val mylist = List[Int](1,2,3,4,5)
    val mylist2 = List[Int]()
    assert(squaredLists.squareList(mylist) == List[Int](1,4,9,16,25))
    assert(squaredLists.squareList2(mylist) == List[Int](1,4,9,16,25))

    assert(squaredLists.squareList(mylist2) == List[Int]())
    assert(squaredLists.squareList2(mylist2) == List[Int]())
  }

  test(testName = "12. Mapped Multiply") {
    val mylist = List[Int](0,1,2,3,4,5)
    val mymap = Map[Int, Int](1 ->2, 3->4)
    assert(mapMultiply.mapMultiple(mylist, mymap) == ArrayBuffer(2, 12))

    val myemptymap = Map[Int, Int]()
    assert(mapMultiply.mapMultiple(mylist, myemptymap) == ArrayBuffer())
  }

  test(testName = "13. Try Retry"){
    def func(): Boolean = {
      val minutesStr = DateTimeFormatter.ofPattern("mm").format(LocalDateTime.now)
      val minutes = minutesStr.toIntOption.get

      if (minutes%2==0) return true
      else throw new Exception("Its an odd time.")
    }
    tryretry.retry(3, 20000) {println(func())}
  }

  test(testName = "14. Gigantic Calc"){
    val myvect = Vector[Int](6,8)
    val gigaObj = new giganticCalc(myvect)

    assert(gigaObj.doGiganticCalc() == (Vector[Int](36, 64), 100, 10.0: Double))
  }

  test(testName = "15. Method Injection"){
    implicit def augmentSequence(s: Seq[Int]): RichSequence = new RichSequence(s)
    val myseq = Seq[Int](10,12,14)
    assert(myseq.median() == 12)

    val myseq2 = Seq[Int](10,12,14,16)
    assert(myseq2.median() == 13)

    val myseq3 = Seq[Int](1)
    assert(myseq3.median() == 1)

  }

//  test("Invoking head on an empty Set should produce NoSuchElementException") {
//    assertThrows[NoSuchElementException] {
//      Set.empty.head
//    }
//  }
}