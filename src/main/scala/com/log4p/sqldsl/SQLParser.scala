package com.log4p.sqldsl

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._

class SQLParser extends JavaTokenParsers {

  def query:Parser[Query] = operation ~ from ~ opt(where) ~ opt(order) ~ opt(limit) ^^ {
    case operation ~ from ~ where ~ order ~ limit => Query(operation, from, where, order, limit)
  }

  def operation:Parser[Operation] = {
    ("select" | "update" | "delete") ~ (("*" ^^^ List("*")) | repsep(ident, ",")) ^^ {
      case "select" ~ f => Select(f:_*)
      case _ => throw new IllegalArgumentException("Operation not implemented")
    }
  }

  def from:Parser[From] = "from" ~> ident ^^ (From(_))

  def where:Parser[Where] = "where" ~> rep(clause) ^^ (Where(_:_*))

  def limit:Parser[Limit] = "limit" ~> wholeNumber ^^ (n => Limit(n.toInt))
  
  def clause:Parser[Clause] = (predicate|parens) * (
            "and" ^^^ { (a:Clause, b:Clause) => And(a,b) } |
            "or" ^^^ { (a:Clause, b:Clause) => Or(a,b) } 
          )

  def parens:Parser[Clause] = "(" ~> clause  <~ ")"
  
  def inString = "(" ~> repsep(stringLiteral, ",") <~ ")"
  def inInt = "(" ~> repsep(int, ",") <~ ")"
  def inDecimal = "(" ~> repsep(decimal, ",") <~ ")"
  def inFloat = "(" ~> repsep(float, ",") <~ ")"
  
  def predicate = (
      ident ~ "=" ~ boolean ^^ { case f ~ "=" ~ b => BooleanEquals(f,b)} 
    | ident ~ "=" ~ stringLiteral ^^ { case f ~ "=" ~ v => StringEquals(f,stripQuotes(v))}
    | ident ~ "=" ~ wholeNumber ^^ { case f ~ "=" ~ i => NumberEquals(f,i.toInt)}
    | ident ~ "in" ~ inString ^^ { case f ~ "in" ~ vals => InString(f,vals:_*)} 
    | ident ~ "in" ~ inInt ^^ { case f ~ "in" ~ vals => InNumber(f,vals.map(i => i.asInstanceOf[Number]):_*)}
    | ident ~ "in" ~ inDecimal ^^ { case f ~ "in" ~ vals => InNumber(f,vals.map(i => i.asInstanceOf[Number]):_*)}
    | ident ~ "in" ~ inFloat ^^ { case f ~ "in" ~ vals => InNumber(f,vals.map(i => i.asInstanceOf[Number]):_*)}

  )

  def boolean = ("true" ^^^ (true) | "false" ^^^ (false))
  def int = wholeNumber ^^ {case n => n.toInt}
  def decimal = decimalNumber ^^ {case n => n.toDouble}
  def float = floatingPointNumber ^^ {case n => n.toFloat}

  def order:Parser[Direction] = {
    "order" ~> "by" ~> ident  ~ ("asc" | "desc") ^^ {
      case f ~ "asc" => Asc(f)
      case f ~ "desc" => Desc(f)
    }
  }

  def stripQuotes(s:String) = s.substring(1, s.length-1)

  def parse(sql:String):Option[Query] = {
    parseAll(query, sql) match {
      case Success(r, q) => Option(r)
      case x => println(x); None
    }
  }
}