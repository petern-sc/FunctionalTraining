package com.rea.higherorder

/**
  * DO NOT ATTEMPT these exercises until you've completed the recursion ones.
  *
  * foldLeft will reduce a list of A's down to a B. It takes an initial value of type B
  * and a list of A's.  It also takes a function which takes the accumulated value of type B
  * and the next value in the list (of type A) and returns a value which will be feed back into
  * the accumulator of the next call.
  *
  * As the name suggests it processes the list from left to right.
  *
  * Have a close look at your implementations from the RecursionExercises.  Which parts could you
  * pull out to a function to make them all common? Your implementation will be very close to
  * foldLeft.
  *
  * Good luck!
  *
  */

object FoldingExercises {
  // why ()() rather than 3 arguments?
  def foldLeft[A, B](initialValue: B, list: List[A])(f: (B, A) => B): B = list match {
    case x :: xs => foldLeft(f(initialValue, x), xs)(f)
    case _ => initialValue
  }

  /**
   * foldRight is the same as foldLeft, except it processes the list from right to left.
   */
  def foldRight[A, B](initialValue: B, list: List[A])(f: (A, B) => B): B = {
//    def go(total: List[A], l: List[A]): List[A] = {
//      l match {
//        case a :: as => go(a :: total, as)
//        case _ => total
//      }
//    }

    val reversedList = foldLeft(Nil: List[A], list)((acc, n) => n :: acc)
//    def goFold(initialValue: B, list: List[A])(f: (A,B) => B): B = {
//      list match {
//        case x :: xs => goFold(f(x, initialValue), xs)(f)
//        case Nil => initialValue
//      }
//    }
//
//    goFold(initialValue, reversedList)(f)
    foldLeft(initialValue, reversedList){(b,a) => f(a,b)}
  }
  /**
   * Remember these, from our recursion exercises?  They can all be implemented with either
   * foldLeft or foldRight.
   */

  def sum(x: List[Int]): Int = foldLeft(initialValue = 0, list = x)(_ + _)

  def length[A](x: List[A]): Int = foldLeft(initialValue = 0, list = x)((acc, _) => acc + 1)

  //Careful you'll need a type annotation on the initialValue field
  // If Nil = List[Nothing], why do we need a type annotation?
  def map[A, B](x: List[A])(f: A => B): List[B] =
    foldRight(initialValue = Nil: List[B], list = x)((next, acc) => f(next) :: acc)

  def filter[A](x: List[A], f: A => Boolean): List[A] =
    foldRight(initialValue = Nil: List[A], list = x)((next, acc) => {
      if (f(next)) {next :: acc}
      else acc
    })

  def append[A](x: List[A], y: List[A]): List[A] =
    foldRight(initialValue = y, list = x)((next, acc) => next :: acc)

  def flatten[A](x: List[List[A]]): List[A] =
    foldLeft(initialValue = Nil: List[A], list = x)(append)

  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] =
    foldRight(initialValue = Nil: List[B], list = x)((next, acc) => append(f(next), acc))

  // Maximum of the empty list is 0
  def maximum(x: List[Int]): Int =
    foldLeft(initialValue = 0, list = x){ (next, acc) =>
      if (next > acc) next
      else acc
    }

  def reverse[A](x: List[A]): List[A] =
    foldLeft(initialValue = Nil: List[A], list = x)((acc, next) => next :: acc)
}
