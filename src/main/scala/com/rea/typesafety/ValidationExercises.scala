package com.rea.typesafety

import scalaz._, Scalaz._

object ValidationExercises {

  def validateKey(key: String, input: Map[String, String]): ValidationNel[ErrorCode, String] = {
    input.get(key) match {
      case None => Failure(keyNotFound(key)).toValidationNel
      case Some(value) => Success(value).toValidationNel
    }
  }

  def nameValidation(name: String, label: String): ValidationNel[ErrorCode, String] =
    if (name.isEmpty) Failure(nameIsEmpty(label)).toValidationNel
    else Success(name).toValidationNel

  def passwordStrengthValidation(password: String): ValidationNel[ErrorCode, String] = {
    val strengthRegex = "\\d".r
    strengthRegex.findFirstIn(password) match {
      case Some(_) => Success(password).toValidationNel
      case None => Failure(passwordTooWeak).toValidationNel
    }
  }

  def passwordLengthValidation(password: String): ValidationNel[ErrorCode, String] = {
    if (password.length > 7) Success(password).toValidationNel
    else Failure(passwordTooShort).toValidationNel
  }

  def validateInput(input: Map[String, String]): ValidationNel[ErrorCode, Person] = {
    val firstName =
      validateKey("firstName", input).flatMap(n => nameValidation(n, "firstName"))

    val lastName =
      validateKey("lastName", input).flatMap(n => nameValidation(n, "lastName"))

    val password = validateKey("password", input).flatMap(p => {
      passwordLengthValidation(p) <* passwordStrengthValidation(p)
    })

    (firstName |@| lastName |@| password) apply Person

    val PersonFunc: String => String => String => Person = (Person.apply _).curried

    password.ap(lastName.ap(firstName.map(PersonFunc)))
    type V[A] = ValidationNel[ErrorCode,A]
    Apply[V].apply3(firstName, lastName, password)(Person)
  }

  def x[F[_],A,B](a:F[A], f1:A=>B)(implicit functor:Applicative[F]): F[B] = functor.map(a)(f1)
}

case class Person(firstName: String, lastName: String, password: String)

sealed trait ErrorCode

case object passwordTooShort extends ErrorCode

case object passwordTooWeak extends ErrorCode

case class keyNotFound(key: String) extends ErrorCode

case class nameIsEmpty(key: String) extends ErrorCode


/*

Interesting Validator combinators

scala> val a:ValidationNel[String,String]  = "hi".successNel
a: scalaz.ValidationNel[String,String] = Success(hi)

scala> val b:ValidationNel[String,String]  = "world".successNel
b: scalaz.ValidationNel[String,String] = Success(world)

scala> val c:ValidationNel[String,String]  = "error1".failNel
c: scalaz.ValidationNel[String,String] = Failure(NonEmptyList(error1))

scala> val d:ValidationNel[String,String]  = "error2".failNel
d: scalaz.ValidationNel[String,String] = Failure(NonEmptyList(error2))

scala> a <* b
res0: scalaz.Validation[scalaz.NonEmptyList[String],String] = Success(hi)

scala> a *> b
res1: scalaz.Validation[scalaz.NonEmptyList[String],String] = Success(world)

scala> c <* d
res2: scalaz.Validation[scalaz.NonEmptyList[String],String] = Failure(NonEmptyList(error1, error2))

scala> a <* d
res3: scalaz.Validation[scalaz.NonEmptyList[String],String] = Failure(NonEmptyList(error2))

scala> a flatMap (hi => b)
res4: scalaz.Validation[scalaz.NonEmptyList[String],String] = Success(world)

scala> a flatMap (s => if (s == "hi") "hey back".successNel else "fine, be that way!".failNel)
res6: scalaz.Validation[scalaz.NonEmptyList[String],String] = Success(hey back)

scala> d flatMap (s => if (s == "hi") "hey back".successNel else "fine, be that way!".failNel)
res7: scalaz.Validation[scalaz.NonEmptyList[String],String] = Failure(NonEmptyList(error2))

scala> b flatMap (s => if (s == "hi") "hey back".successNel else "fine, be that way!".failNel)
res8: scalaz.Validation[scalaz.NonEmptyList[String],String] = Failure(NonEmptyList(fine, be that way!))

scala> a map (hi => hi + " worldz")
res5: scalaz.Validation[scalaz.NonEmptyList[String],String] = Success(hi worldz)


 */
