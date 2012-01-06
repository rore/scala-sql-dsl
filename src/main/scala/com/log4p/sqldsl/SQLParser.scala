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

  def from:Parser[From] = "from" ~> identifier ^^ (From(_))

  def where:Parser[Where] = "where" ~> rep(clause) ^^ (Where(_:_*))

  def limit:Parser[Limit] = "limit" ~> wholeNumber ^^ (n => Limit(n.toInt))
  
  def clause:Parser[Clause] = (predicate|parens) * (
            "and" ^^^ { (a:Clause, b:Clause) => And(a,b) } |
            "or" ^^^ { (a:Clause, b:Clause) => Or(a,b) } 
          )

  def parens:Parser[Clause] = "(" ~> clause  <~ ")"
  
  def inString = "(" ~> repsep(stringLiteral, ",") <~ ")"
  //def inInt = "(" ~> repsep(int, ",") <~ ")"
  //def inDecimal = "(" ~> repsep(decimal, ",") <~ ")"/
  //def inFloat = "(" ~> repsep(float, ",") <~ ")"
  def inNumber = "(" ~> repsep(number, ",") <~ ")"
  
  def predicate = (
      identifier ~ "=" ~ boolean ^^ { case f ~ "=" ~ b => BooleanEquals(f,b)} 
    | identifier ~ "=" ~ stringLiteral ^^ { case f ~ "=" ~ v => StringEquals(f,stripQuotes(v))}
    | identifier ~ ">" ~ stringLiteral ^^ { case f ~ ">" ~ v => GTString(f,stripQuotes(v))}
    | identifier ~ ">=" ~ stringLiteral ^^ { case f ~ ">=" ~ v => GTEString(f,stripQuotes(v))}
    | identifier ~ "<" ~ stringLiteral ^^ { case f ~ "<" ~ v => LTString(f,stripQuotes(v))}
    | identifier ~ "<=" ~ stringLiteral ^^ { case f ~ "<=" ~ v => LTEString(f,stripQuotes(v))}
    | identifier ~ "=" ~ number ^^ { case f ~ "=" ~ i => NumberEquals(f,i)}
    | identifier ~ ">" ~ number ^^ { case f ~ ">" ~ v => GTNumber(f,v)}
    | identifier ~ ">=" ~ number ^^ { case f ~ ">=" ~ v => GTENumber(f,v)}
    | identifier ~ "<" ~ number ^^ { case f ~ "<" ~ v => LTNumber(f,v)}
    | identifier ~ "<=" ~ number ^^ { case f ~ "<=" ~ v => LTENumber(f,v)}
    | identifier ~ "in" ~ inString ^^ { case f ~ "in" ~ vals => InString(f,vals:_*)} 
    | identifier ~ "in" ~ inNumber ^^ { case f ~ "in" ~ vals => InNumber(f,vals:_*)}

  )

  def identifier = ident | (stringLiteral ^^ {case i => stripQuotes(i)})
  def boolean = ("true" ^^^ (true) | "false" ^^^ (false))
  def int = wholeNumber ^^ {case n => n.toInt}
  def decimal = decimalNumber ^^ {case n => n.toDouble}
  def float = floatingPointNumber ^^ {case n => n.toFloat}
  def number = (decimal ^^ {case n => n.asInstanceOf[Number]} | float ^^ {case n => n.asInstanceOf[Number]} | int ^^ {case n => n.asInstanceOf[Number]}) 
  
  def order:Parser[Direction] = {
    "order" ~> "by" ~> ident  ~ ("asc" | "desc") ^^ {
      case f ~ "asc" => Asc(f)
      case f ~ "desc" => Desc(f)
    }
  }

  def stripQuotes(s:String) = if (null != s && s.startsWith("\"")) s.substring(1, s.length-1) else s;

  var lastError:Option[String] = None;
  
  def parse(sql:String):Option[Query] = {
    lastError = None
    val res = parseAll(query, sql) 
    res match {
      case Success(r, q) => Option(r)
      case x => {
    	  lastError = Some(x.toString); 
    	  println(lastError.get); 
    	  None
      }
    }
  }
}