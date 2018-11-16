package com.rea.parser

import com.rea.parser.Parser._

object Parser {
  def pure[A](a: A): Parser[A] = new Parser[A] {
    override def parse(s: String): Either[String, A] = Right(a)
  }

}

trait Parser[A] {

  def parse(s: String): Either[String, A]

  def map[B](f: A => B): Parser[B] = {
    val self = this

    new Parser[B] {
      override def parse(s: String): Either[String, B] = self.parse(s).right.map(f)
    }
  }

  def mapNew[B](f: A => B): Parser[B] = {
    // need something from B => Parser[B]
    flatMap(a => pure(f(a)))
  }

  // flatMap(f: A => F[B])
  // flatMap(a => pure(b))


  // implement map with flatmap and pure
  // need to implement a pure

  //

  def or(parse2: Parser[A]): Parser[A] = {
    val self = this

    new Parser[A] {
      override def parse(s: String): Either[String, A] = self.parse(s) match {
        case Right(a) => Right(a)
        case Left(_) => parse2.parse(s)
      }
    }
  }

  def and[B](parse2: Parser[B]): Parser[(A, B)] = {
    this.map2[B, (A, B)]((a, b) => (a, b), parse2)
  }

  def getFirst[B](parser1: Parser[A], parser2: Parser[B]): Parser[A] = {
    parser1.map2((a, _: B) => a, parser2)
  }

  def flatMap[B](f: A => Parser[B]): Parser[B] = {
    val self = this
    new Parser[B] {
      override def parse(s: String): Either[String, B] = {
        self.parse(s) match {
          case Right(a) => f(a).parse(s)
          case Left(error) => Left(error)
        }
      }
    }
  }

  def map2[B, C](f: (A, B) => C, parse2: Parser[B]): Parser[C] = {
    this.flatMap(a => parse2.map(b => f(a, b)))
  }

}

object Parsers {
  def oneParser: Parser[Int] = new Parser[Int] {
    override def parse(s: String): Either[String, Int] =
      if (s contains "one") Right(1) else Left("could not find one")
  }

  def twoParser: Parser[Int] = new Parser[Int] {
    override def parse(s: String): Either[String, Int] =
      if (s contains "two") Right(2) else Left("could not find two")
  }

  def stringParser: Parser[String] = new Parser[String] {
    override def parse(s: String): Either[String, String] =
      if (s contains "peter") Right("peter") else Left("could not find peter")
  }


  def oneOrTwoParser: Parser[Int] = {
    val x: Parser[Int] = oneParser.or(twoParser)
    x
  }

  def number(p: Parser[Int]): Parser[String] = p.mapNew(number => s"You have $number things")


  // F[A] -> (A -> B): F[B]
  //        .map(number => s"You have $number things")

}
