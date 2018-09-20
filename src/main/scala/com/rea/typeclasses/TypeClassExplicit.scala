package com.rea.typeclasses

object TypeClassExplicit {

  trait Show[A] {
    def show(a: A): String
  }

  case class Person(firstName: String, lastName: String, age: Int)

  val stringInstance: Show[String] = new Show[String] {
    override def show(a: String): String = a
  }

  def optionShowInstance[A](aInstance: Show[A]): Show[Option[A]] = new Show[Option[A]] {
    override def show(option: Option[A]): String = option match {
      case Some(x) => s"We have a value, it is: " + aInstance.show(x)
      case None => "There is nothing here"
    }
  }

  val integerInstance: Show[Int] = new Show[Int] {
    override def show(b: Int): String = b.toString
  }

  val personShowInstance: Show[Person] = new Show[Person] {
    override def show(person: Person): String =
        s"First Name: ${stringInstance.show(person.firstName)} " +
          s"Last Name: ${stringInstance.show(person.lastName)} " +
          s"is ${integerInstance.show(person.age)} years old"
  }

  def display[A](a: A)(show: Show[A]): Unit = {
    println(show.show(a))
  }

}
