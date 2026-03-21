import it.unibo.pps.u03.*
import u03.Sequences.*
import it.unibo.pps.u02.Modules.Person
import it.unibo.pps.u02.Modules.Person.*

object Task2:

  import Sequence.*

  def foldLeft[A, B](s: Sequence[A])(initial: B)(op: (B, A) => B): B = s match
    case Cons(h, t) => foldLeft(t)(op(initial, h))(op)
    case Nil()      => initial

  def teacherCourses(persons: Sequence[Person]): Sequence[String] =
    Sequence.flatMap(persons) {
      case Teacher(_, course) => Cons(course, Nil())
      case _                  => Nil()
    }

  def countDistinctTeacherCourses(persons: Sequence[Person]): Int =
    val courses = teacherCourses(persons)
    val uniqueCourses = Sequence.distinct(courses)
    foldLeft(uniqueCourses)(0)((acc, _) => acc + 1)

@main def tryTask2 =
  import Sequence.*

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

  println(Task2.teacherCourses(persons))
  println(Task2.countDistinctTeacherCourses(persons))