package com.rea.typeclasses

import argonaut.Argonaut._
import argonaut.{DecodeJson, DecodeResult, Json, _}

object MonadTypeClass {
  trait Monad[M[_]] {

    def pure[A](a: A): M[A]

    def flatMap[A, B](m: M[A])(f: A => M[B]): M[B]

  }

  implicit val optionMonad = new Monad[Option] {
    override def pure[A](a: A): Option[A] = Some(a)

    override def flatMap[A, B](m: Option[A])(f: A => Option[B]): Option[B] = m.flatMap(f)
  }

  implicit val listMonad = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)

    override def flatMap[A, B](m: List[A])(f: A => List[B]): List[B] = m.flatMap(f)
  }


  def makeMeAMonad[M[A], A](a: A)(m: Monad[M]): M[A] = {
    m.pure(a)
  }

  def combine[M[_], A, B, C](ma: M[A], mb: M[B])(f: (A, B) => C)(implicit m: Monad[M]): M[C] =
    m.flatMap(ma) { a =>
      m.flatMap(mb) { b =>
        m.pure(f(a,b))
      }
    }

  val json: String =
    """{
      |  "firstName": "Tom",
      |  "lastName": "Baker"
      |}""".stripMargin

  val decodeJsonMonad: Monad[DecodeJson] = new Monad[DecodeJson] {
    override def pure[A](a: A): DecodeJson[A] = DecodeJson(c => DecodeResult.ok(a))

    override def flatMap[A, B](m: DecodeJson[A])(f: A => DecodeJson[B]): DecodeJson[B] = m.flatMap(f)
  }

  val firstNameDecoder:DecodeJson[String] = DecodeJson(c => (c --\ "firstName").as[String])
  val lastNameDecoder:DecodeJson[String]  = DecodeJson(c => (c --\ "lastName").as[String])

  val nameDecoder = combine(firstNameDecoder, lastNameDecoder)((first, last) => s"$first $last")(decodeJsonMonad)

  val output: Option[String] = json.decodeOption(nameDecoder)
}
