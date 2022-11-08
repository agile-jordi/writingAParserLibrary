package com.agilogy.wapl

import Json.{JsonArray, JsonBoolean, JsonNumber, JsonString}

object JsonParser:

  private val whiteSpaceChars = Set(' ', '\n', '\r', '\t')

  val whitespace: Parser[Unit] = (s, position) =>
    Right(() -> (position + s.substring(position).takeWhile(c => whiteSpaceChars.contains(c)).length))

  val jsonTrue: Parser[JsonBoolean] = token("true").map(_ => JsonBoolean(true))
  val jsonFalse: Parser[JsonBoolean] = token("false").map(_ => JsonBoolean(false))

  val boolean: Parser[JsonBoolean] = jsonTrue | jsonFalse

  val string: Parser[JsonString] = regex("string", "\"[^\"]*\"".r)
    .map(s =>  JsonString(s.substring(1, s.length - 1)))

  val number: Parser[JsonNumber] =
    regex("number", "-?([1-9][0-9]*|0)(\\.[0-9]+)?([eE][\\-+]?[0-9]+)?".r).map(JsonNumber.apply)

  val array: Parser[Json] =
    token("[") **
      ((json ** (token(",") ** json).repeated).map { case (b, l) => JsonArray(b :: l) } | whitespace.map(_ => JsonArray(List.empty)))
      ** token("]")

  val json: Parser[Json] = boolean | string | number | array