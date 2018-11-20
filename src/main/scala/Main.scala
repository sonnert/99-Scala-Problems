import scala.util.Random
import scala.collection.mutable.MultiMap
import scala.collection.immutable.ListMap

object Main extends App {
  val l = List('a, 'a, 'a, 'a, 'b, 'c, 'c, 'a, 'a, 'd, 'e, 'e, 'e, 'e)
  val l2 = List('a, 'b, 'c, 'd)
  val l3 = List('x, 'y, 'z)
  val l4 = List('p, 'q, 'w)
  println("List used: "+l)

  // P2
  def penultimate[A](l: List[A]): Any = {
    l match {
      case Nil => Nil
      case e :: _ :: Nil => e
      case _ :: tail => penultimate(tail)
    }
  }
  println("P2: "+penultimate(l))

  // P3
  def nth[A](n: Int, l: List[A]): Any = {
    l(n-1)
  }
  println("P3 (n=5): "+nth(5, l))

  // P6
  def is_palindrome[A](l: List[A]): Boolean = {
    l.foldLeft(List[A]())((l, r) => r :: l).equals(l)
  }
  println("P6: "+is_palindrome(l))

  // P8
  def compress(l: List[Any]): List[Any] = {
    def compress_inner(l: List[Any], last: Any): List[Any] = {
      l match {
        case Nil => Nil
        case first :: rest if first == last => compress_inner(rest, first)
        case first :: rest => first :: compress_inner(rest, first)
      }
    }
    compress_inner(l, Nil)
  }
  println("P8: "+compress(l))
  
  // P9
  def pack[A](l: List[A]): List[List[A]] = {
    val (prefix, suffix) = l.span(_ == l.head)
    if (suffix == Nil)
      List(prefix)
    else
      prefix :: pack(suffix)
  }
  println("P9: "+pack(l))

  // P10
  def encode10[A](l: List[A]): List[(Int, A)] = {
    pack(l).map(x => (x.size, x.head))
  }
  println("P10: "+encode10(l))

  // P11
  def encode11[A](l: List[A]): List[Any] = {
    pack(l).map(x => if (x.size == 1) x.head else (x.size, x.head))
  }
  println("P11: "+encode11(l))

  // P12
  def decode(l: List[Any]): List[Any] = {
    encode10(l).flatMap(x => List.fill(x._1)(x._2))
  }
  println("P12: "+decode(l))

  // P14
  def duplicate[A](l: List[A]): List[A] = {
    l.flatMap(x => List(x, x))
  }
  println("P14: "+duplicate(l))

  // P15
  def duplicate_n[A](l: List[A], n: Int): List[A] = {
    l.flatMap(x => List.fill(n)(x))
  }
  println("P15 (3): "+duplicate_n(l, 3))

  // P16
  def drop[A](l: List[A]): List[A] = {
    l.sliding(3, 4).toList.flatten
  }
  println("P16 :"+drop(l))

  // P19
  def rotate[A](n: Int, l: List[A]): List[A] = {
    l.drop(l.size-n) ::: l.dropRight(n)
  }
  println("P19 (3): "+rotate(3, l))

  // P20
  def removeAt[A](n: Int, l: List[A]): List[A] = {
    l.dropRight(l.size-n+1) ::: l.drop(n)
  }
  println("P20 (5): "+removeAt(5, l))

  // P21
  def insertAt[A](newe: A, n: Int, l: List[A]): List[A] = {
    l.take(n-1) ::: newe :: l.drop(n-1)
  }
  println("P21 (3): "+insertAt('new, 3, l))

  // P23
  def randomSelect[A](n: Int, l: List[A]): List[A] = {
    List.fill(n)(l(Random.nextInt(l.size)))
  }
  println("P23 (5): "+randomSelect(5, l))

  // P24
  def lotto(n: Int, s: Int): List[Int] = {
    List.fill(n)(Random.nextInt(s))
  }
  println("P24 (5, 69): "+lotto(5, 69))

  // P25
  def randomPermute[A](l: List[A]): List[A] = {
    randomSelect(l.size, l)
  }
  println("P25: "+randomPermute(l))

  // P27
  def group[A](nl: List[Int], l: List[A]): List[List[Any]] = {
    nl match {
      case Nil => List(Nil)
      case first :: rest => l.combinations(first).toList.flatMap(
        x => group(rest, l.diff(x)).map(x :: _)
      )
    }
  }
  println("P27: "+group(List(2,1), List('a, 'b, 'c)))

  // P28
  // a
  def lsort[A](l: List[List[A]]): List[List[A]] = {
    l.sortWith(_.length < _.length)
  }
  println("P28a: "+lsort(List(l3, l2)))
  //b
  def lsortFreq[A](l: List[List[A]]): List[List[A]] = {
    val lengthed = l.map(e => (e.size, e))
    val mapped = lengthed.groupBy(e => e._1).mapValues(e => e.map(x => x._2))
    mapped.toList.map(x => x._2).sortWith(_.size < _.size).flatten
  }
  println("P28b: "+lsortFreq(List(l4, l2, l3)))

  /*
  // P
  def name[A](l: List[A]): List[A] = {
    ???
  }
  println("P: ")
  */

}