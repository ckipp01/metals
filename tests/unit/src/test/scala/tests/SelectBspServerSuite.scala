package tests

import scala.meta.internal.metals.Messages
import scala.meta.internal.metals.MetalsEnrichments._

import ch.epfl.scala.bsp4j.BspConnectionDetails
import munit.Location

class SelectBspServerSuite extends BaseSuite {
  def check(
      name: String,
      currentlyUsing: Option[String],
      candidates: List[BspConnectionDetails],
      expected: String
  )(implicit loc: Location): Unit = {
    test(name) {
      val query = Messages.SelectBspServer.request(candidates, currentlyUsing)
      val obtained =
        query.params.getActions.asScala.map(_.getTitle).mkString("\n")
      assertNoDiff(obtained, expected)
    }
  }

  def name(n: String, version: String = "1.0.0"): BspConnectionDetails = {
    new BspConnectionDetails(n, List().asJava, version, "2.0.0", List().asJava)
  }

  check(
    "basic",
    None,
    List(
      name("Bloop"),
      name("Mill")
    ),
    """
      |Bloop
      |Mill
      |""".stripMargin
  )

  check(
    "with-label",
    Some("Bloop"),
    List(
      name("Bloop"),
      name("Mill")
    ),
    """
      |Bloop (currently using)
      |Mill
      |""".stripMargin
  )

  check(
    "version",
    None,
    List(
      name("Bloop", "1.0"),
      name("Bloop", "2.0")
    ),
    """
      |Bloop v1.0
      |Bloop v2.0
      |""".stripMargin
  )

  check(
    "conflict",
    None,
    List(
      name("Bloop", "1.0"),
      name("Bloop", "1.0")
    ),
    """
      |Bloop v1.0 (a)
      |Bloop v1.0 (b)
      |""".stripMargin
  )
}
