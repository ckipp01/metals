package scala.meta.internal.pc

import org.eclipse.lsp4j.SelectionRange
import scala.meta.pc.OffsetParams
import java.{util => ju}

class SelectionRangeProvider(
    val compiler: MetalsGlobal,
    params: ju.List[OffsetParams]
) {
  import compiler._

  def selectionRanges(): List[SelectionRange] = {
    import scala.collection.JavaConverters._
    // val position = params.getPositions.asScala.head

    val headPosition = params.asScala.toList.head

    val unit = addCompilationUnit(
      headPosition.text(),
      headPosition.uri().toString(),
      None
    )

    pprint.log(params)
    pprint.log(headPosition)
    pprint.log(unit)

    val pos = unit.position(headPosition.offset())
    pprint.log(pos)
    // val tree = locateTree(pos)

    val tree = parseTree(
      unit.source
    )

    pprint.log(tree)
    pprint.log(tree.pos)
    pprint.log(tree.pos.end)
    pprint.log(tree.pos.start)
    pprint.log(tree.pos.isRange)

    List.empty[SelectionRange]
  }
}
