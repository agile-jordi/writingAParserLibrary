package com.agilogy.wapl

import Json.*

import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class JsonParserTest extends AnyFunSuite :

  import JsonParser.*

  test("Parse empty array") {
    assert(array("[]") == Right(JsonArray(List.empty)))
  }

  private val random = new Random()

  test("Parse empty array with whitespace") {
    val ws = " " * random.between(1, 5)
    assert(array(s"[$ws]") == Right(JsonArray(List.empty)))
  }

  test("Parse empty array failure, missing ]") {
    val input = "["
    assert(array(input) == Left(ParseError(input, 1, List("\"]\""))))
  }

  test("Parse empty array failure, unexpected content") {
    val input = "[]wut?"
    assert(array(input) == Left(ParseError(input, 2, List("end of file"))))
  }

  test("Parse token") {
    assert(token("[")("[") == Right(()))
  }

  test("Parse token failure") {
    val input = "notTheStartArrayToken"
    assert(token("[")(input) == Left(ParseError(input, 0, List("\"[\""))))
  }

  test("Parse boolean") {
    assert(boolean("true") == Right(JsonBoolean(true)))
    assert(boolean("false") == Right(JsonBoolean(false)))
  }

  test("Parse boolean failure") {
    val input = "notABoolean"
    assert(boolean(input) == Left(ParseError(input, 0, List("\"true\"", "\"false\""))))
  }

  test("Parse array of boolean values") {
    val length = random.between(10000, 10005)
    val booleans = (0 until length).map(_ => random.nextBoolean()).toList
    assert(array(s"[${booleans.map(_.toString).mkString(",")}]") ==
      Right(JsonArray(booleans.map(JsonBoolean.apply)))
    )
  }

  test("Parse array of values and arrays") {
    assert(array("[1,false,\"hello\",[true,3]]") ==
      Right(JsonArray(List(
        JsonNumber("1"),
        JsonBoolean(false),
        JsonString("hello"),
        JsonArray(List(JsonBoolean(true), JsonNumber("3")))
      ))))
  }

  test("Parse json numbers") {
    assert(number("1") == Right(JsonNumber("1")))
    assert(number("0.1") == Right(JsonNumber("0.1")))
    assert(number("-0.1") == Right(JsonNumber("-0.1")))
    assert(number("-0.1") == Right(JsonNumber("-0.1")))
    assert(number("-0.1e2") == Right(JsonNumber("-0.1e2")))
    assert(number("-0.1e+2") == Right(JsonNumber("-0.1e+2")))
    assert(number("-0.1e-2") == Right(JsonNumber("-0.1e-2")))
  }

  test("Parse json strings") {
    assert(string("\"\"") == Right(JsonString("")))
    assert(string("\"Json\"") == Right(JsonString("Json")))
    assert(string("\"No només ASCII\"") == Right(JsonString("No només ASCII")))
    assert(string("\"\u0020\udbff\udfff\"") == Right(JsonString("\u0020\udbff\udfff")))
  }


