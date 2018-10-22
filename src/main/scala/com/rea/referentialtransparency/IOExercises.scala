package com.rea.referentialtransparency

import scalaz._, Scalaz._
import scalaz.effect.IO
import scala.io.StdIn.readLine

object IOExercises {

//  sealed trait Done
//  object Done extends Done

  def readLineFromConsole(): IO[String] = IO(readLine)

//  def writeToConsole(s: String): IO[Done] = IO{println(s); Done}
  def writeToConsole(s: String): IO[Unit] = IO{println(s)}

//  def readFromConsoleAndWriteToConsole: IO[Done] = readLineFromConsole().flatMap(writeToConsole)
  def readFromConsoleAndWriteToConsole: IO[Unit] = readLineFromConsole().flatMap(writeToConsole)

  def run(effect: IO[Unit]): Unit = effect.unsafePerformIO()

}
