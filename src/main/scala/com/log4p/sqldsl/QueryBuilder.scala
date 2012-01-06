package com.log4p.sqldsl

case class Query(val operation:Operation, val from: From, val where: Option[Where], val order: Option[Direction] = None,
		val limit: Option[Limit] = None) {
  def order(dir: Direction): Query = this.copy(order = Option(dir))
  def limit(num:Int): Query = this.copy(limit = Option(Limit(num)))
}

abstract class Operation {
  def from(table: String) = From(table, Option(this))
}
case class Select(val fields:String*) extends Operation
case class From(val table: String, val operation:Option[Operation] = None) {
  def where(clauses: Clause*): Query = Query(operation.get, this, Option(Where(clauses: _*)))
}

case class Where(val clauses: Clause*)

abstract class Clause {
  def and(otherField: Clause): Clause = And(this, otherField)
  def or(otherField: Clause): Clause = Or(this, otherField)
}

abstract class In(val field:String, val values:Any*) extends Clause

case class StringEquals(val field: String, val value: String) extends Clause
case class NumberEquals(val field: String, val value: Number) extends Clause
case class BooleanEquals(val field: String, val value: Boolean) extends Clause
case class GTNumber(val field: String, val value: Number) extends Clause
case class GTENumber(val field: String, val value: Number) extends Clause
case class LTNumber(val field: String, val value: Number) extends Clause
case class LTENumber(val field: String, val value: Number) extends Clause
case class GTString(val field: String, val value: String) extends Clause
case class GTEString(val field: String, val value: String) extends Clause
case class LTString(val field: String, val value: String) extends Clause
case class LTEString(val field: String, val value: String) extends Clause
case class InString(override val field: String, override val values: String*) extends In(field, values)
case class InNumber(override val field: String, override val values: Number*) extends In(field, values)
case class InBoolean(override val field: String, override val values: Boolean*) extends In(field, values)
case class And(val lClause:Clause, val rClause:Clause) extends Clause
case class Or(val lClause:Clause, val rClause:Clause) extends Clause

abstract class Direction
case class Asc(field: String) extends Direction
case class Desc(field: String) extends Direction

case class Limit(limit:Int) 

object QueryBuilder {
  implicit def tuple2field(t: (String, String)): StringEquals = StringEquals(t._1, t._2)
  implicit def tuple2field(t: (String, Int)): NumberEquals = NumberEquals(t._1, t._2)
  implicit def tuple2field(t: (String, Boolean)): BooleanEquals = BooleanEquals(t._1, t._2)
  implicit def from2query(f: From): Query = Query(f.operation.get, f, Option(Where()))

  /** entrypoint for starting a select query */
  def select(fields:String*) = Select(fields:_*)
  def select(symbol: Symbol): Select = symbol match {
    case 'all => select("*")
    case _ => throw new RuntimeException("Only 'all allowed as symbol")
  }

  def in(field: String, values: String*) = InString(field, values: _*)
}