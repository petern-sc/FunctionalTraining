package com.rea.recursion

// Taken from http://tmorris.net/posts/scala-exercises-for-beginners/index.html

/**
 * Ok here are the rules.
 *
 * You can't use any of the standard list functions, like `map`, `filter`, `flatMap`, `append`, `:::`, `:+`, etc.
 *
 * But you can always use `::` to construct a new list by prepending an element to another list.
 *
 * You CAN and are encouraged to use the solutions from the exercises below to solve the harder
 * ones towards the end.
 *
 * Keep an eye out for repetition and similarities between your answers.
 *
 * REMEMBER: Follow the types, they almost always guide you to the solution.  If it compiles and looks a little
 * too simple, it's probably correct.  As Sherlock Holmes once said, "Each one is suggestive, together they are
 * most certainly conclusive."
 *
 * See if you can make your solution tail recursive, where possible.
 *
 */

object RecursionExercises {

  def plusOne(n: Int) = n + 1

  def minusOne(n: Int) = n - 1

  // Add two non-negative Integers together.  You are only allowed to use plusOne and minusOne above
  def add(a: Int, b: Int): Int = if (b > 0)
    add(plusOne(a), minusOne(b))
  else
    a

  // You are not permitted to use any list functions such as map, flatMap, ++, flatten etc
  def sum(l: List[Int]): Int = {
    def go(total: Int, l: List[Int]): Int = {
      l match {
        case x :: xs => go(total + x ,xs)
        case _ => total
      }
    }
    go(0, l)
  }

//    l match {
//    case x :: xs => add(x, sum(xs)) // x + sum(xs)
//    case _ => 0
//  }

  //Again no list functions are permitted for the following
  def length[A](x: List[A]): Int = {
    def go(total: Int, l: List[A]): Int = {
      l match {
        case _ :: xs => go(plusOne(total), xs)
        case _ => total
      }
    }
    go(0, x)
  }

//    x match {
//    case _ :: xs => plusOne(length(xs))
//    case _ => 0
//  }

  // Do you notice anything similar between sum and length? Hmm...

  // Mapping over a list.  You are given a List of type A and a function converting an A to a B
  // and you give back a list of type B.  No list functions allowed!
//  def map[A, B](x: List[A], f: A => B): List[B] = {
//    def go(total: List[B], l: List[A]): List[B] = {
//      l match {
//        case x :: xs => go(f(x) :: total, xs)
//        case _ => total
//      }
//    }
//    go(Nil, x)
//  }

  def map[A, B](x: List[A], f: A => B): List[B] = {
    x match {
      case x :: xs => f(x) :: map(xs, f)
      case _ => Nil
    }
  }

  // Given a function from A => Boolean, return a list with only those item where the function returned true.
//  def filter[A](x: List[A], f: A => Boolean): List[A] = {
//    def go(total: List[A], l: List[A]): List[A] = {
//      l match {
//        case a :: as => if (f(a)) go(a :: total, as) else go(total, as)
//        case _ => total
//      }
//    }
//    go(Nil, x)
//  }
    def filter[A](x: List[A], f: A => Boolean): List[A] = x match {
      case x :: xs => if (f(x))
        x :: filter(xs, f)
      else filter(xs, f)
      case _ => Nil
    }

  // This pattern should be familiar by now... psst... look at add.
  def append[A](x: List[A], y: List[A]): List[A] = x match {
    case a :: as => a :: append(as, y)
    case _ => y
  }

  // Flatten a list of lists to a single list.  Remember you can't use list.flatten.  Can you use a previous
  // solution to solve this one?
  def flatten[A](x: List[List[A]]): List[A] = {
    def go(total: List[A], l: List[List[A]]): List[A] = {
      l match {
        case xs :: rest => go(append(total, xs), rest)
        case _ => total
      }
    }
    go(Nil, x)
  }

//    x match {
//    case xs :: rest => append(xs, flatten(rest))
//    case _ => Nil
//  }

  // Follow the types.  You've done a great job getting here. Follow the types.
  def flatMap[A, B](x: List[A], f: A => List[B]): List[B] = flatten(map(x, f))

//  {
//    def go(total: List[B], l: List[A]): List[B] = {
//      l match {
//        case a :: as => go(append(total, f(a)), as)
//        case _ => total
//      }
//    }
//    go(Nil, x)
//  }


//    x match {
//    case a :: as => append(f(a), flatMap(as, f))
//    case _ => Nil
//  }
  // peter: from list POV is map == flatmap?, output
  // pattern matching on constructor?

  // Maximum of the empty list is 0
  def maximum(x: List[Int]): Int = x match {
    case a :: as => if (a > maximum(as)) a else maximum(as)
    case _ => 0
  }

  // Reverse a list
  def reverse[A](x: List[A]): List[A] = {
    def go(total: List[A], l: List[A]): List[A] = {
      l match {
        case a :: as => go(a :: total, as)
        case _ => total
      }
    }
    go(Nil, x)
  }

//    x match {
//    case a :: as => append(reverse(as), a :: Nil)
//    case _ => Nil
//  }
}
