package com.agilogy.wapl

import scala.annotation.targetName
import com.agilogy.wapl.{sequence => _sequence}

type Parser[A] = (String, Int) => Either[ParseError, (A, Int)]

def string(token: String): Parser[Unit] = (s, position) =>
  if (s.startsWith(token, position)) Right(() -> (position + token.length))
  else Left(ParseError(s, position, List(s"\"$token\"")))

def empty[A](value: A): Parser[A] = (_, position) => Right(value -> position)

def sequence[A, B](a: Parser[A], b: Parser[B]): Parser[(A, B)] = (s, position) =>
  for
    aI0 <- a(s, position)
    (a, i0) = aI0
    bI1 <- b(s, i0)
    (b, i1) = bI1
  yield (a -> b) -> i1

implicit class UnitParserOps(self: Parser[Unit]):
  infix def sequence[B](other: Parser[B]): Parser[B] =
    _sequence(self, other).map(_._2)
  def **[B](other: Parser[B]): Parser[B] =
    _sequence(self, other).map(_._2)

implicit class ParserOps[A](self: Parser[A]):

  def apply(s: String): Either[ParseError, A] =
    self(s, 0) match
      case Right((_, endPosition)) if endPosition < s.length =>
        Left(ParseError(s, endPosition, List("end of file")))
      case r => r.map(_._1)

  def map[B](f: A => B): Parser[B] = (s, position) =>
    self(s, position).map((a, finalPosition) => f(a) -> finalPosition)

  def |(other: Parser[A]): Parser[A] = (s, position) =>
    self(s, position) match
      case Right(a) => Right(a)
      case Left(e1) => other(s, position) match
        case Right(a) => Right(a)
        case Left(e2) => Left(e2.copy(expected = e1.expected ++ e2.expected))

  infix def sequence[B](other: Parser[B]): Parser[(A, B)] = _sequence(self, other)
  @targetName("sequenceUnit")
  infix def sequence(other: Parser[Unit]): Parser[A] = _sequence(self, other).map(_._1)

  def **[B](other: Parser[B]): Parser[(A, B)] = sequence(other)
  @targetName("starStarUnit")
  def **(other: Parser[Unit]): Parser[A] = sequence(other)

  def repeated: Parser[List[A]] = (self ** repeated).map(_ :: _) | empty(List.empty)