package com.agilogy.wapl

import Json.{JsonArray, JsonBoolean, JsonNumber, JsonString, JsonNull, JsonObject}

object JsonParser:

  private val whiteSpaceChars = Set(' ', '\n', '\r', '\t')

  private val whitespace: Parser[Unit] = (s, position) =>
    Right(() -> (position + s.substring(position).takeWhile(c => whiteSpaceChars.contains(c)).length))

  private val jsonTrue: Parser[JsonBoolean] = "true".as(JsonBoolean(true))
  private val jsonFalse: Parser[JsonBoolean] = "false".as(JsonBoolean(false))

  val jsonNull: Parser[JsonNull.type]  = "null".as(JsonNull)

  val boolean: Parser[JsonBoolean] = jsonTrue | jsonFalse

  val string: Parser[JsonString] = regex("string", "\"[^\"]*\"".r)
    .map(s =>  JsonString(s.substring(1, s.length - 1)))

  val number: Parser[JsonNumber] =
    regex("number", "-?([1-9][0-9]*|0)(\\.[0-9]+)?([eE][\\-+]?[0-9]+)?".r).map(JsonNumber.apply)

  //noinspection ForwardReference
  val array: Parser[Json] =
    "[" ** whitespace ** json.separatedBy(",").map(JsonArray.apply) ** token("]")

  //noinspection ForwardReference
  val member: Parser[(String, Json)] = string.map(_.value) ** token(":") ** json

  val obj: Parser[Json] =
    "{" ** member.separatedBy(",").map(members => JsonObject(members.toMap)) ** "}"

  val json: Parser[Json] = boolean | string | number | jsonNull | array | obj