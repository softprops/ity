package ity

object Consumer {
  sealed trait Input[+In]
  case class Chunk[In](input: In) extends Input[In]
  case object Empty extends Input[Nothing]
  case object EOF extends Input[Nothing]

  sealed trait State[In, Out]
  case class Done[In, Out](value: Out, remaining: Input[In]) extends State[In, Out]
  case class Continue[In, Out](next: Consumer[In, Out], remaining: Input[In]) extends State[In, Out]  
  case class Error[In, Out](error: Throwable) extends State[In, Out]
}

trait Consumer[In, Out] { self =>
  import Consumer._

  def apply(in: Input[In]): State[In, Out]

  def flatMap[NextOut](f: Out => Consumer[In, NextOut])(
    implicit manifest: Manifest[NextOut]): Consumer[In, NextOut] =
    new Consumer[In, NextOut] {
      def apply(in: Input[In]): State[In, NextOut] =
        self(in) match {
          case Continue(next, remaining) =>
            Continue(next.flatMap(f), remaining)
          case Done(value, remaining) =>
            Continue(f(value), remaining)
          case Error(e) => Error(e)
        }
    }

  def map[NextOut](f: Out => NextOut)(
    implicit manifest: Manifest[NextOut]): Consumer[In, NextOut] =
    new Consumer[In, NextOut] {
      def apply(in: Input[In]): State[In, NextOut] =
        self(in) match {
          case Continue(next, remaining) =>
            Continue(next.map(f), remaining)
          case Done(value, remaining) =>
            Done(f(value), remaining)
          case Error(e) => Error(e)
        }
    }

  def all(xs: List[In]): Either[Throwable, Out] =
    self(if (xs.isEmpty) EOF else Chunk(xs.head)) match {
      case Continue(next, EOF) => next.all(Nil)
      case Continue(next, Empty) => next.all(xs.tail)
      case Continue(next, Chunk(c)) => next.all(c :: xs.tail)
      case Done(value, _) => Right(value)
      case Error(e) => Left(e)
    }
}
