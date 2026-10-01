//
// Moped - my own private IDE-aho
// https://github.com/samskivert/moped/blob/master/LICENSE

package moped.major

import moped._
import moped.code.{CodeConfig, Commenter, BlockIndenter}
import moped.grammar._

@Major(name="sql",
       tags=Array("code", "project", "sql"),
       pats=Array(".*\\.sql"),
       desc="A major editing mode for SQL.")
class SqlMode (env :Env) extends SitterCodeMode(env) {
  import CodeConfig._

  override def dispose () :Unit = {} // nada for now

  override def langId = new org.treesitter.TreeSitterSql()

  override def styles = {
    // the grammar models type names as keywords (`keyword_int`, `keyword_varchar`, etc.), but we'd
    // rather style them as types; local to this method for the same reason as TypeScriptMode's
    // keyword set (SitterCodeMode's constructor calls `styles` before our fields are initialized)
    val typeKeywords = Set(
      "bigint", "bigserial", "binary", "bit", "boolean", "box2d", "box3d", "bytea", "char",
      "character", "date", "datetime", "datetime2", "datetimeoffset", "decimal", "double", "float",
      "geography", "geometry", "image", "inet", "int", "interval", "json", "jsonb", "mediumint",
      "money", "nchar", "numeric", "nvarchar", "oid", "precision", "real", "regclass",
      "regnamespace", "regproc", "regtype", "serial", "smalldatetime", "smallint", "smallmoney",
      "smallserial", "string", "text", "time", "timestamp", "timestamptz", "tinyint", "uuid",
      "varbinary", "varchar", "varying", "xml")

    typeKeywords.map(kw => s"keyword_$kw" -> always(typeStyle)).toMap ++ Map(
      "comment" -> always(commentStyle),
      "marginalia" -> always(commentStyle),

      // literals include numbers, strings and TRUE/FALSE/NULL (the latter as `keyword_*` children of
      // a `literal` node); we style them all alike
      "literal" -> always(constantStyle),
      "keyword_true" -> always(constantStyle),
      "keyword_false" -> always(constantStyle),
      "keyword_null" -> always(constantStyle),
      "parameter" -> always(variableStyle),

      "identifier" -> (scopes => scopes.head match {
        case "object_reference" => scopes.tail.headOption match {
          case Some("invocation") => functionStyle
          case _ => typeStyle // table, schema, function and type names
        }
        case "column_definition" => variableStyle
        case "function_argument" => variableStyle
        case _ => null
      }),
    )}

  override def fallbackStyles = {
    case tpe if tpe.startsWith("keyword_") => always(keywordStyle)
  }

  override def syntaxes = Map(
    "comment" -> (_ => Syntax.LineComment),
    "marginalia" -> (_ => Syntax.BlockComment),
    "literal" -> (_ => Syntax.StringLiteral),
  )

  override protected def createIndenter () = new BlockIndenter(config, Seq(
    new BlockIndenter.BlockCommentRule()
  ))

  override val commenter = new Commenter() {
    override def linePrefix  = "--"
    override def blockOpen   = "/*"
    override def blockClose  = "*/"
    override def blockPrefix = "*"
  }
}
