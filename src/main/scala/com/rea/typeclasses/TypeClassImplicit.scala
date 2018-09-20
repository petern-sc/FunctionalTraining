package com.rea.typeclasses

object TypeClassImplicit {

  trait Show[A] {
    def show(a: A): String
  }

  case class Person[A: Show](firstName: String, lastName: String, age: Int, attribute1: A)

  implicit def optionShowInstance[A](implicit aInstance: Show[A]): Show[Option[A]] = new Show[Option[A]] {
    override def show(option: Option[A]): String = option match {
      case Some(a) => s"We have a value and it is: " + aInstance.show(a)
      case None    => "We found nothing"
    }
  }

  implicit val stringInstance: Show[String] = new Show[String] {
    override def show(a: String): String = a
  }

  implicit val integerInstance: Show[Int] = new Show[Int] {
    override def show(b: Int): String = b.toString
  }

  implicit def personShowInstance[A](implicit sInstance: Show[String], iInstance: Show[Int], aInstance: Show[A]): Show[Person[A]] =
    new Show[Person[A]] {
      override def show(person: Person[A]): String =
        s"First Name: ${sInstance.show(person.firstName)} " +
          s"Last Name: ${sInstance.show(person.lastName)} " +
          s"is ${iInstance.show(person.age)} years old " +
          s"special interest: ${aInstance.show(person.attribute1: A)}"
    }

  implicit def listShowInstance[A](implicit aInstance: Show[A]): Show[List[A]] = new Show[List[A]] {
    override def show(list: List[A]): String = list match {
      case Nil => "This list is completely empty"
      case _ => s"The list is [" + list.map(aInstance.show).mkString(", ") + "]"
    }
  }

  def display[A](a: A)(implicit show: Show[A]): Unit = {
    println(show.show(a))
  }

}
