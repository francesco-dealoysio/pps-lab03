package u03

import u03.Optionals.*
import u03.Optionals.Optional

object Sequences: // Essentially, generic linkedlists

  import u03.Optionals.Optional.{Empty, Just}

  enum Sequence[E]:
    case Cons(head: E, tail: Sequence[E])
    case Nil()

  object Sequence:

    def sum(l: Sequence[Int]): Int = l match
      case Cons(h, t) => h + sum(t)
      case _          => 0

    def map[A, B](l: Sequence[A])(mapper: A => B): Sequence[B] = l match
      case Cons(h, t) => Cons(mapper(h), map(t)(mapper))
      case Nil()      => Nil()

    def filter[A](l1: Sequence[A])(pred: A => Boolean): Sequence[A] = l1 match
      case Cons(h, t) if pred(h) => Cons(h, filter(t)(pred))
      case Cons(_, t)            => filter(t)(pred)
      case Nil()                 => Nil()

    // Lab 03

    /*
     * Skip the first n elements of the sequence
     * E.g., [10, 20, 30], 2 => [30]
     * E.g., [10, 20, 30], 3 => []
     * E.g., [10, 20, 30], 0 => [10, 20, 30]
     * E.g., [], 2 => []
     */
    def skip[A](s: Sequence[A])(n: Int): Sequence[A] = s match
      case Cons(h, t) if n > 0 => skip(t)(n - 1)
      case Cons(h, t)          => Cons(h, t)
      case Nil()               => Nil()

    /*
     * Zip two sequences
     * E.g., [10, 20, 30], [40, 50] => [(10, 40), (20, 50)]
     * E.g., [10], [] => []
     * E.g., [], [] => []
     */

    /* Idea: Devi prendere:
    - testa di first
    - testa di second
    - fare una coppia
    - continuare sui tail*/
    def zip[A, B](first: Sequence[A], second: Sequence[B]): Sequence[(A, B)] = (first, second) match
        case (Cons(h1, t1), Cons(h2, t2)) => Cons((h1, h2), zip(t1, t2))
        case _ => Nil()

    /*
     * Concatenate two sequences
     * E.g., [10, 20, 30], [40, 50] => [10, 20, 30, 40, 50]
     * E.g., [10], [] => [10]
     * E.g., [], [] => []
     */
    /*Idea: attacca s2 alla fine di s1
    * se s1 è vuota → ritorni s2
    * altrimenti ricostruisci la lista
    */
    def concat[A](s1: Sequence[A], s2: Sequence[A]): Sequence[A] = s1 match
        case Cons(h, t) => Cons(h, concat(t, s2))
        case Nil()      => s2

    /*
     * Reverse the sequence
     * E.g., [10, 20, 30] => [30, 20, 10]
     * E.g., [10] => [10]
     * E.g., [] => []
     */

    /* Idea: prendi ogni elemento e lo metti davanti a una nuova lista */
    def reverse[A](s: Sequence[A]): Sequence[A] =
      def rev(curr: Sequence[A], acc: Sequence[A]): Sequence[A] = curr match
        case Cons(h, t) => rev(t, Cons(h, acc))
        case Nil() => acc

      rev(s, Nil())

    /*
    curr = lista da processare
    acc = lista già invertita

    Inizio: curr = [10, 20, 30]
            acc  = []

    Step 1:
    Prendi 10
    acc = [10]
    curr = [20, 30]

    Step 2:
    Prendi 20
    acc = [20, 10]
    curr = [30]

    Step 3:
    Prendi 30
    acc = [30, 20, 10]
    curr = []

    Fine: acc = [30, 20, 10]
     */


    /*
     * Map the elements of the sequence to a new sequence and flatten the result
     * E.g., [10, 20, 30], calling with mapper(v => [v, v + 1]) returns [10, 11, 20, 21, 30, 31]
     * E.g., [10, 20, 30], calling with mapper(v => [v]) returns [10, 20, 30]
     * E.g., [10, 20, 30], calling with mapper(v => Nil()) returns []
     */
    /* Applicare mapper -> ottieni una lista
    *  Concateni tutti
    */
    def flatMap[A, B](s: Sequence[A])(mapper: A => Sequence[B]): Sequence[B] =
      s match
        case Cons(h, t) => concat(mapper(h), flatMap(t)(mapper))
        case Nil()      => Nil()

    /*
     * Get the minimum element in the sequence
     * E.g., [30, 20, 10] => Just(10)
     * E.g., [10, 1, 30] => Just(1)
     * E.g., [] => Empty()
     */
    def min(s: Sequence[Int]): Optional[Int] =
      def searchMin(s: Sequence[Int], oldMin: Int): Int = s match
        case Cons(h, t) =>
          val newMin = if (h < oldMin) h else oldMin
          searchMin(t, newMin)
        case _ =>
          oldMin

      val result = searchMin(s, Int.MaxValue)
      result match
      case Int.MaxValue => Empty()
      case _ => Just(result)

    /*
     * Get the elements at even indices
     * E.g., [10, 20, 30] => [10, 30]
     * E.g., [10, 20, 30, 40] => [10, 30]
     */
    def evenIndices[A](s: Sequence[A]): Sequence[A] =
      def even(curr: Sequence[A], acc: Sequence[A], n: Int): Sequence[A] = curr match
        case Cons(h, t) if n % 2 == 0 => even(t, Cons(h, acc), n + 1)
        case Cons(h, t) => even(t, acc, n + 1)
        case Nil() => acc

      reverse(even(s, Nil(), 0))

    /*
     * Check if the sequence contains the element
     * E.g., [10, 20, 30] => true if elem is 20
     * E.g., [10, 20, 30] => false if elem is 40
     */
    def contains[A](s: Sequence[A])(elem: A): Boolean = s match
      case Cons(h, t) => h == elem || contains(t)(elem)
      case Nil()      => false

    /*
     * Remove duplicates from the sequence
     * E.g., [10, 20, 10, 30] => [10, 20, 30]
     * E.g., [10, 20, 30] => [10, 20, 30]
     */
    def distinct[A](s: Sequence[A]): Sequence[A] =
      def dist(curr: Sequence[A], acc: Sequence[A]): Sequence[A] = curr match
        case Cons(h, t) =>
          if contains(acc)(h) then dist(t, acc)
          else dist(t, Cons(h, acc))
        case Nil() => acc

      reverse(dist(s, Nil()))

    /*
     * Group contiguous elements in the sequence
     * E.g., [10, 10, 20, 30] => [[10, 10], [20], [30]]
     * E.g., [10, 20, 30] => [[10], [20], [30]]
     * E.g., [10, 20, 20, 30] => [[10], [20, 20], [30]]
     */
    def group[A](s: Sequence[A]): Sequence[Sequence[A]] = ???

    /*
     * Partition the sequence into two sequences based on the predicate
     * E.g., [10, 20, 30] => ([10], [20, 30]) if pred is (_ < 20)
     * E.g., [11, 20, 31] => ([20], [11, 31]) if pred is (_ % 2 == 0)
     */
    def partition[A](s: Sequence[A])(pred: A => Boolean): (Sequence[A], Sequence[A]) =
      val neg: (A => Boolean) => (A => Boolean) = f => (s => !f(s))
      (filter(s)(pred), filter(s)(neg(pred)))

@main def trySequences =
  import Sequences.* 
  val l = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(30, Sequence.Nil())))
  val l2 = Sequence.Cons(40, Sequence.Cons(50, Sequence.Cons(60, Sequence.Nil())))
  println(Sequence.sum(l)) // 30

  import Sequence.*

  println(sum(map(filter(l)(_ >= 20))(_ + 1))) // 21+31 = 52
  println(sum(skip(l)(3)))

  val pairs: Sequence[(Int, String)] = Sequence.Cons((10, "ten"), Sequence.Cons((20, "twenty"), Sequence.Cons((30, "thirty"), Sequence.Nil())))

  println(pairs)
  val l3 = Sequence.Cons(10, Sequence.Cons(20, Sequence.Cons(10, Sequence.Nil())))
  println(filter(l3)(_ > 10))



//zip(l, l2)