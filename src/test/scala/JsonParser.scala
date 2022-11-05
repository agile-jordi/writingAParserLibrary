package com.agilogy.wapl

import Json.{JsonArray, JsonBoolean}

object JsonParser:

  private val whiteSpaceChars = Set(' ', '\n', '\r', '\t')

  val whitespace: Parser[Unit] = (s, position) =>
    Right(() -> (position + s.substring(position).takeWhile(c => whiteSpaceChars.contains(c)).length))

  val array: Parser[JsonArray] =
    (string("[") ** whitespace ** string("]"))
      .map(_ => JsonArray(List.empty))

  val jsonTrue: Parser[JsonBoolean] = string("true").map(_ => JsonBoolean(true))
  val jsonFalse: Parser[JsonBoolean] = string("false").map(_ => JsonBoolean(false))

  val boolean: Parser[JsonBoolean] = jsonTrue | jsonFalse

  val booleanArray: Parser[JsonArray] =
    (string("[") ** boolean ** string(",") ** boolean ** string("]"))
      .map { case ((((_, b1), _), b2), _) => JsonArray(List(b1, b2)) }