package scala.meta.internal.mtags

import java.io.StringReader
import java.net.URI
import java.util.Comparator
import java.util.Optional

import scala.util.control.NonFatal

import scala.meta.inputs.Input
import scala.meta.inputs.Position
import scala.meta.internal.jdk.CollectionConverters._
import scala.meta.internal.metals.CompilerRangeParamsUtils
import scala.meta.internal.metals.EmptyCancelToken
import scala.meta.internal.metals.Report
import scala.meta.internal.mtags.ScalametaCommonEnrichments._
import scala.meta.internal.semanticdb.Language
import scala.meta.internal.semanticdb.SymbolInformation.Kind
import scala.meta.internal.semanticdb.SymbolInformation.Property
import scala.meta.pc.reports.ReportContext

import com.thoughtworks.qdox._
import com.thoughtworks.qdox.model.JavaClass
import com.thoughtworks.qdox.model.JavaConstructor
import com.thoughtworks.qdox.model.JavaField
import com.thoughtworks.qdox.model.JavaMember
import com.thoughtworks.qdox.model.JavaMethod
import com.thoughtworks.qdox.model.JavaModel
import com.thoughtworks.qdox.parser.ParseException
import org.eclipse.lsp4j

object JavaMtags {
  def index(
      input: Input.VirtualFile,
      includeMembers: Boolean
  )(implicit rc: ReportContext): MtagsIndexer =
    new JavaMtags(input, includeMembers)
}
class JavaMtags(virtualFile: Input.VirtualFile, includeMembers: Boolean)(
    implicit rc: ReportContext
) extends MtagsIndexer { self =>
  val builder = new JavaProjectBuilder()
  override def language: Language = Language.JAVA

  override def input: Input.VirtualFile = virtualFile

  override def indexRoot(): Unit = {
    try {
      val source = builder.addSource(new StringReader(input.value))
      if (source.getPackage != null) {
        source.getPackageName.split("\\.").foreach { p =>
          pkg(
            p,
            toRangePosition(source.getPackage.lineNumber, p)
          )
        }
      }
      source.getClasses.asScala.foreach(visitClass)
    } catch {
      case e: ParseException =>
        reportError(
          "parse error",
          e,
          Some(new lsp4j.Position(e.getLine() - 1, e.getColumn()))
        )
      case e: NullPointerException =>
        reportError("null pointer exception", e, None)
    }
  }

  /**
   * Computes the start/end offsets from a name in a line number.
   *
   * Applies a simple heuristic to find the name: the first occurence of
   * name in that line. If the name does not appear in the line then
   * 0 is returned. If the name appears for example in the return type
   * of a method then we get the position of the return type, not the
   * end of the world.
   */
  def toRangePosition(line: Int, name: String): Position = {
    val offset = input.toOffset(line, 0)
    val columnAndLength = {
      val fromIndex = {
        // HACK(olafur) avoid hitting on substrings of "package".
        if (input.value.startsWith("package", offset)) "package".length
        else offset
      }
      val idx = input.value.indexOf(" " + name, fromIndex)
      if (idx == -1) (0, 0)
      else (idx - offset + " ".length, name.length)
    }
    input.toPosition(
      line,
      columnAndLength._1,
      line,
      columnAndLength._1 + columnAndLength._2
    )
  }

  def visitMembers[T <: JavaMember](cls: JavaClass): Unit = {
    val fields = cls.getFields
    if (fields == null) ()
    else fields.asScala.foreach(visitMember(cls, _))
  }

  def visitClasses(classes: java.util.List[JavaClass]): Unit =
    if (classes == null) ()
    else classes.asScala.foreach(visitClass)

  def visitClass(
      cls: JavaClass,
      pos: Position,
      kind: Kind
  ): Unit = {
    tpe(
      cls.getName,
      pos,
      kind,
      if (cls.isEnum) Property.ENUM.value else 0
    )
  }

  def visitClass(cls: JavaClass): Unit =
    withOwner(owner) {
      val kind = if (cls.isInterface) Kind.INTERFACE else Kind.CLASS
      val pos = toRangePosition(cls.lineNumber, cls.getName)
      visitClass(
        cls,
        pos,
        kind
      )
      visitClasses(cls.getNestedClasses)
      if (includeMembers) {
        visitMethods(cls)
        visitConstructors(cls)
        visitMembers(cls)
      }
    }

  def visitConstructor(
      ctor: JavaConstructor,
      disambiguator: String,
      pos: Position,
      properties: Int
  ): Unit = {
    super.ctor(disambiguator, pos, properties)
  }

  def visitConstructors(cls: JavaClass): Unit = {
    val overloads = new OverloadDisambiguator()
    cls.getConstructors
      .iterator()
      .asScala
      .filterNot(_.isPrivate)
      .foreach { ctor =>
        val name = cls.getName
        val disambiguator = overloads.disambiguator(name)
        val pos = toRangePosition(ctor.lineNumber, name)
        withOwner() {
          visitConstructor(ctor, disambiguator, pos, 0)
        }
      }
  }

  def visitMethod(
      method: JavaMethod,
      name: String,
      disambiguator: String,
      pos: Position,
      properties: Int
  ): Unit = {
    super.method(name, disambiguator, pos, properties)
  }

  def visitMethods(cls: JavaClass): Unit = {
    val overloads = new OverloadDisambiguator()
    val methods = cls.getMethods
    methods.sort(new Comparator[JavaMethod] {
      override def compare(o1: JavaMethod, o2: JavaMethod): Int = {
        java.lang.Boolean.compare(o1.isStatic, o2.isStatic)
      }
    })
    methods.asScala.foreach { method =>
      val name = method.getName
      val disambiguator = overloads.disambiguator(name)
      val line =
        if (method.lineNumber == -1) cls.lineNumber else method.lineNumber
      val pos = toRangePosition(line, name)
      withOwner() {
        visitMethod(method, name, disambiguator, pos, 0)
      }
    }
  }

  def visitMember[T <: JavaMember](cls: JavaClass, m: T): Unit =
    withOwner(owner) {
      val name = m.getName
      val line = m match {
        case c: JavaMethod => c.lineNumber
        case c: JavaField => c.lineNumber
        case _ => 0
      }
      val actualLine =
        if (line == -1) cls.lineNumber
        else line
      val pos = toRangePosition(actualLine, name)
      val (kind: Kind, properties: Int) = m match {
        case _: JavaMethod => (Kind.METHOD, 0)
        case field: JavaField if field.isEnumConstant() =>
          (Kind.FIELD, Property.ENUM.value)
        case _: JavaField =>
          (Kind.FIELD, 0)
        case c: JavaClass =>
          if (c.isInterface) (Kind.INTERFACE, 0)
          else (Kind.CLASS, 0)
        case _ => (Kind.UNKNOWN_KIND, 0)
      }
      term(name, pos, kind, properties)
    }

  implicit class XtensionJavaModel(m: JavaModel) {
    def lineNumber: Int = m.getLineNumber - 1
  }

  private def reportError(
      errorName: String,
      e: Exception,
      position: Option[lsp4j.Position]
  ) = {
    try {
      val content = position
        .flatMap(_.toMeta(virtualFile))
        .map(pos =>
          CompilerRangeParamsUtils.fromPos(pos, EmptyCancelToken).printed()
        )
        .map(content => s"""|
                            |file content:
                            |```java
                            |$content
                            |```
                            |""".stripMargin)
        .getOrElse("")

      val shortFileName = {
        val index = virtualFile.path.indexOf("jar!")
        if (index > 0) virtualFile.path.substring(index + 4)
        else virtualFile.path
      }

      rc.unsanitized()
        .create(() =>
          new Report(
            name = "qdox-error",
            text = s"""|error in qdox parser$content
                       |""".stripMargin,
            error = Some(e),
            path = Optional.of(new URI(virtualFile.path)),
            shortSummary = s"QDox $errorName in $shortFileName",
            id = Optional.of(virtualFile.path)
          )
        )
    } catch {
      case NonFatal(_) =>
    }
  }

}
