package it.unibo.pps.u03

import org.junit.*
import org.junit.Assert.*

class Task2Test:
  import it.unibo.pps.u03.Task2.*
  import it.unibo.pps.u02.Modules.Person
  import it.unibo.pps.u02.Modules.Person.*
  import u03.Sequences.*
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

  @Test def testFoldLeft() = {
    val lst = Cons(3, Cons(7, Cons(1, Cons(5, Nil()))))
    assertEquals(-16, foldLeft(lst)(0)(_ - _))
    assertEquals(26, foldLeft(lst)(10)(_ + _))
  }

  @Test def testTeacherCourses() =
    val courses = Cons("PPS",Cons("PPS",Cons("PCD",Nil())))
    assertEquals(courses, (teacherCourses(persons)))
    assertEquals(Nil(), teacherCourses(Nil()))

  @Test def testCountDistinctTeacherCourses() =
    assertEquals(2,countDistinctTeacherCourses(persons))
    assertEquals(0, countDistinctTeacherCourses(Nil()))



