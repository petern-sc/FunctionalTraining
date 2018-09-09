package com.rea.typesafety

/**
  * Use pattern matching and recursion.  No vars, no loops, no overriding.
  *
  * `Option` is an implementation of optional functionality.
  *
  * We've made these exercises to give you greater insight into how an optional pattern
  * might work in a functional language.
  *
  * When you see `Option` think: "It may exist, or it may not"
  *
  * There are two ways to construct an `Option`:
  *
  *   `Some()` represents something that exists
  *
  *   `None` represents something that doesn't exist
  *
  * We use `Option` in situations where there isn't certainty that a meaningful
  * value will be returned to us.
  *
  *   The `get()` method on the key to value store `Map` is a great example of this.
  *
  *     We expect `get()` to take a key and give us a value in return.
  *
  *     But what happens when our Map doesn't know about the key we gave it?
  *
  *   A Map here is the same as in any other language,
  *   we just need to tell it about the types we're working with.
  *
  *     This is the type of the key
  *                      |
  *                      |     This is the type of the value
  *                      |      |
  *                      |      |
  *     val myMap = Map[Int, String]( 1 -> "one", 2 -> "two, ...)
  *
  *
  *   When we call `get()` on Map we will always get back an `Option` type
  *
  *     myMap.get(1) = Some("one")  //The value exists and it's the string "one"
  *
  *     myMap.get(0) = None      //The value doesn't exist so we get None
  *
  *   `Some("one")` and `None` are both of the type Option
  *
  *   Since `Some` and `None` are the same type we can pattern match on them!
  *
  *   We can have one set of logic when we get Some back and a different set
  *   of logic when we get `None` back!
  *
  *   val mightBeSomething: Option[String] = myMap.get(3)
  *
  *   val result: String = mightBeSomething match {
  *     case Some(string) => "I got a String back!"
  *     case None => "I got None back"
  *   }
  *
  * Good luck!
  *
  */

object OptionalExercises1 {

  val config = Map[String, String]("host" -> "rea.com", "port" -> "8080")

  def getFromConfig(key: String): Option[String] = config.get(key)

  def lengthOfHost(): Option[Int] = getFromConfig(key = "host").map(_.length())

  def portPlus1000(): Option[Int] = getFromConfig(key = "port").map(_.toInt).map(_ + 1000)

  def add1000(number: Int): Int = number + 1000
}

object OptionalExercises2 {

  val hosts = Map("host1" -> "rea.com", "host2" -> "test.rea.com", "host3" -> "netflix.com")
  val envs = Map("rea.com" -> "prod", "test.rea.com" -> "test", "amazon.com" -> "stage")

  // Should return the host string if successful or "couldn't resolve" if unsuccessful
  def getEnvForHost(host: String): String =
    hosts.get(host).flatMap(envs.get).getOrElse("couldn't resolve")

  // See how many ways you can implement this.
  // Will either return "Connected to <rea host>" or "not connected"

  /** 1 **/
  //  def connectToReaHostsOnly(host: String): String = hosts.get(host) match {
  //    case Some(env) => reaHostsOnly(env = env)
  //    case None => "not connected"
  //  }
  //
  //  def reaHostsOnly(env: String): String = {
  //    if (env contains "rea") {
  //      createConnection(domain = env)
  //    } else {
  //      "not connected"
  //    }
  //  }


  /** 2 **/
  def connectToReaHostsOnly(host: String): String = hosts.get(host).flatMap(isReaHost).map(createConnection).getOrElse("not connected")
  def connectToReaHostsOnly2(host: String): String = hosts.get(host).filter(_.contains("rea")).map(createConnection).getOrElse("not connected")

  def isReaHost(env: String): Option[String] = if (env contains "rea") Some(env) else None

  def createConnection(domain: String): String = s"connected to $domain"
}

/**
  * Here we make the trait `Maybe`, which is our version of `Option`
  *
  * `Just` has the same behavior as `Some`
  * `Nothing` has the same behavior as `None`
  *
  * We use this exercise to illustrate that we can create our own optional behavior
  * with just a few functions.
  *
  */

object OptionalExercises3 {

  sealed trait Maybe[+A]

  case class Just[A](get: A) extends Maybe[A]

  case object Nothing extends Maybe[Nothing]

  def flatMap[A, B](m: Maybe[A])(f: A => Maybe[B]): Maybe[B] = m match {
    case Just(a) => f(a)
    case Nothing => Nothing
  }

  def map[A, B](m: Maybe[A])(f: A => B): Maybe[B] = m match {
    case Just(a) => Just(f(a))
    case Nothing => Nothing
  }

  def fold[A, B](m: Maybe[A], default: => B, f: A => B): B = m match {
    case Just(a) => f(a)
    case Nothing => default
  }

  def orElse[A](m: Maybe[A], otherwise: => Maybe[A]): Maybe[A] = m match {
    case b@Just(_) => b
    case Nothing => otherwise
  }

  def orSome[A](m: Maybe[A], default: => A): A = m match {
    case Just(a) => a
    case Nothing => default
  }

  def map2[A, B, C](f: (A, B) => C)(m1: Maybe[A], m2: Maybe[B]): Maybe[C] = {
//    val function2: Maybe[B => C] = map(m1)(a => f(a, _))
//    ap(m2, function2)

    flatMap(m1)(a => map(m2)(b => f(a,b)))
  }

  def sequence[A](l: List[Maybe[A]]): Maybe[List[A]] = {
    l.foldRight(Just(Nil): Maybe[List[A]])((maybeNext, maybeAcc) => {
      flatMap(maybeNext)(next =>
        map(maybeAcc)(accList =>
          next :: accList))
    })
  }
//    def miniSequence(list: List[Maybe[A]], acc: Maybe[List[A]]): Maybe[List[A]] = {
//      list match {
//        case head :: tail => miniSequence(tail, flatMap(head)(a => map(acc)(li => a :: li)))
//        case Nil => acc
//      }
//    }
//    miniSequence(l, Just(Nil): Maybe[List[A]])

  def ap[A, B](m1: Maybe[A], m2: Maybe[A => B]): Maybe[B] = m2 match {
    case Just(f) => map(m1)(f)
    case _ => Nothing
  }
}
