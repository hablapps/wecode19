package org.hablapps.wecode19
package code

import scala.concurrent.{Await, Future, ExecutionContext, duration}
import ExecutionContext.Implicits.global, duration._
import scala.io.StdIn.readLine
import org.scalatest._

object `I-FunctionalAPIs`{

  // Functional API

  trait IO[P[_]]{
    def read(): P[String]
    def write(msg: String): P[Unit]

    // Imperative combinators
    def returns[A](a: A): P[A]
    def doAndThen[A,B](p: P[A])(f: A => P[B]): P[B]
  }

  // Particular APIs for particular computation types can now be
  // defined in a modular way

  // Asynchronous computations
  type AsyncIO = IO[Future]

  // State-transformations
  case class IOState(reads: List[String], writes: List[String])
  type IOTrans[T] = IOState => (IOState,T)
  type StateIO = IO[IOTrans]

  // Synchronous transformations
  type Id[T] = T
  type SynchIO = IO[Id]
}


object `II-Instantiations`{
  import `I-FunctionalAPIs`._

  // Asynchronous computation

  object AsyncIO extends IO[Future]{
    def read() = ??? // Whatever
    def write(msg: String) = ??? // Whatever

    def returns[A](a: A): Future[A] =
      Future.successful(a)

    def doAndThen[A,B](a: Future[A])(f: A => Future[B]): Future[B] =
      a flatMap f
  }

  // Console-based interpretation of IO Programs

  object ConsoleIO extends IO[Id]{
    def read() = readLine()
    def write(msg: String) = println(msg)

    def returns[A](a: A): A = a

    def doAndThen[A,B](a: A)(f: A => B) = f(a)
  }

  // State transformations

  object StateIO extends IO[IOTrans]{
    def read(): IOTrans[String] = {
      case IOState(msg::readsTail, writes) =>
        (IOState(readsTail, writes), msg)
      case _ => throw new Exception("not enough data to be read")
    }

    def write(msg: String): IOTrans[Unit] = {
      case IOState(reads, writes) =>
        (IOState(reads, msg::writes), ())
    }

    def doAndThen[A,B](p: IOTrans[A])(f: A => IOTrans[B]): IOTrans[B] =
      iostate1 => p(iostate1) match {
        case (iostate2, a) => f(a)(iostate2)
      }

    def returns[A](a: A): IOTrans[A] =
      iostate => (iostate, a)
  }
}

object `III-Logic`{
  import `I-FunctionalAPIs`._

  def echo[P[_]]()(io: IO[P]): P[String] =
    io.doAndThen(io.read()){ msg =>
      io.doAndThen(io.write(msg)){ _ =>
        io.returns(msg)
      }
    }
}

object `IV-Composition`{
  import `I-FunctionalAPIs`.IOState
  import `II-Instantiations`.{ConsoleIO, StateIO, AsyncIO}
  import `III-Logic`.echo

  def consoleEcho(): String =
    echo()(ConsoleIO)

  def stateEcho(): IOState => (IOState, String) =
    echo()(StateIO)

  def asyncEcho(): Future[String] =
    echo()(AsyncIO)
}
