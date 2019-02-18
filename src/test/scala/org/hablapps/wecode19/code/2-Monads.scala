package org.hablapps.wecode19
package code.monads

import scala.concurrent.{Await, Future, ExecutionContext, duration}
import ExecutionContext.Implicits.global, duration._
import scala.io.StdIn.readLine
import org.scalatest._

object `I-FunctionalAPIs`{

  // Functional APIs

  trait IO[P[_]]{
    def read(): P[String]
    def write(msg: String): P[Unit]
  }

  trait Monad[P[_]]{
    def returns[A](a: A): P[A]
    def flatMap[A,B](p: P[A])(f: A => P[B]): P[B]
  }
}

object `II-Instantiations`{
  import `I-FunctionalAPIs`._

  // Asynchronous computation

  object AsyncIO extends IO[Future]{
    def read() = ??? // Whatever
    def write(msg: String) = ??? // Whatever
  }

  object AsyncMonad extends Monad[Future]{
    def returns[A](a: A): Future[A] =
      Future.successful(a)

    def flatMap[A,B](a: Future[A])(f: A => Future[B]): Future[B] =
      a flatMap f
  }

  // Console-based interpretation of IO Programs

  type Id[T] = T

  object ConsoleIO extends IO[Id]{
    def read() = readLine()
    def write(msg: String) = println(msg)
  }

  object SyncMonad extends Monad[Id]{
    def returns[A](a: A): A = a

    def flatMap[A,B](a: A)(f: A => B) = f(a)
  }

  // State transformations

  case class IOState(read: List[String], written: List[String])

  type IOTrans[T] = IOState => (IOState, T)

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
  }

  object StateMonad extends Monad[IOTrans]{

    def flatMap[A,B](p: IOTrans[A])(f: A => IOTrans[B]): IOTrans[B] =
      iostate1 => p(iostate1) match {
        case (iostate2, a) => f(a)(iostate2)
      }

    def returns[A](a: A): IOTrans[A] =
      iostate => (iostate, a)
  }
}

object `III-Logic`{
  import `I-FunctionalAPIs`._

  def echo[P[_]]()(io: IO[P], m: Monad[P]): P[String] =
    m.flatMap(io.read()){ msg =>
      m.flatMap(io.write(msg)){ _ =>
        m.returns(msg)
      }
    }
}

object `IV-Composition`{
  import `II-Instantiations`._
  import `III-Logic`.echo

  def consoleEcho(): String =
    echo()(ConsoleIO, SyncMonad)

  def stateEcho(): IOState => (IOState, String) =
    echo()(StateIO, StateMonad)

  def asyncEcho(): Future[String] =
    echo()(AsyncIO, AsyncMonad)
}
