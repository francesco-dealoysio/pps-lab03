package it.unibo.pps.u03

import scala.annotation.tailrec

object Task2:
  import it.unibo.pps.u02.Modules.Person
  import it.unibo.pps.u02.Modules.Person.*
  import u03.Sequences.*
  import Sequence.*

  @tailrec
  def foldLeft[A, B](s: Sequence[A])(initial: B)(op: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(op(initial, h))(op)
    case Nil()      => initial

  def teacherCourses(persons: Sequence[Person]): Sequence[String] =
    Sequence.flatMap(persons) {
      case Teacher(_, course) => Cons(course, Nil())
      case _ => Nil()
    }

  def countDistinctTeacherCourses(persons: Sequence[Person]): Int =
    val courses = teacherCourses(persons)
    val distinctCourses = Sequence.distinct(courses)
    foldLeft(distinctCourses)(0)((acc, _) => acc + 1)

@main def tryTask2 =
  import it.unibo.pps.u02.Modules.Person
  import it.unibo.pps.u02.Modules.Person.*
  import u03.Sequences.*
  import Sequence.*

  val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))

  val persons: Sequence[Person] =
    Cons(Teacher("Viroli", "PPS"),
      Cons(Student("Mario", 2015),
        Cons(Teacher("Aguzzi", "PPS"),
          Cons(Teacher("Ricci", "PCD"),
            Nil()
          )
        )
      )
    )

  println(s"\nfoldLeft............: ${Task2.foldLeft(lst)(0)(_ - _)}") // -16
  println("teacherCourses......: " + Task2.teacherCourses(persons)) // Cons("PPS",Cons("PPS",Cons("PCD",Nil())))
  println("countDistinctCourses: " + Task2.countDistinctTeacherCourses(persons)) // 2