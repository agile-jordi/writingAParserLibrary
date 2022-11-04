package com.agilogy.wapl

case class ParseError(input: String, position: Int, expected: List[String])
