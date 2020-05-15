package scala.meta.internal.metals

import java.sql.Connection
import JdbcEnrichments._

class ChosenBuildTool(conn: () => Connection) {
  def selectedBuildTool(): Option[String] = {
    conn().query(
      "select * from chosen_build_tool LIMIT 1;"
    )(_ => ()){x =>
      val y = x.getString("build_tool")
      pprint.log(y)
      y
    }.headOption
  }
  def chooseBuildTool(buildTool: String) = {
    conn().update {
      "insert into chosen_build_tool values (?);"
    } { stmt => stmt.setString(1, buildTool) }
  }
}
