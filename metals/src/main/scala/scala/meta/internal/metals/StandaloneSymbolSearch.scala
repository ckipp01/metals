package scala.meta.internal.metals

import java.nio.file.Path
import java.{util => ju}

import scala.collection.concurrent.TrieMap

import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.mtags.OnDemandSymbolIndex
import scala.meta.internal.mtags.Symbol
import scala.meta.io.AbsolutePath
import scala.meta.pc.SymbolDocumentation
import scala.meta.pc.SymbolSearch
import scala.meta.pc.SymbolSearch.Result
import scala.meta.pc.SymbolSearchVisitor

import org.eclipse.lsp4j.Location

class StandaloneSymbolSearch(
    workspace: AbsolutePath,
    classpath: Seq[AbsolutePath],
    sources: Seq[AbsolutePath],
    buffers: Buffers,
    isExcludedPackage: String => Boolean,
    workspaceFallback: Option[SymbolSearch] = None
) extends SymbolSearch {

  private val dependencySourceCache =
    new TrieMap[AbsolutePath, ju.List[String]]()
  private val classpathSearch =
    ClasspathSearch.fromClasspath(
      classpath.toList.map(_.toNIO),
      isExcludedPackage
    )

  private val index = OnDemandSymbolIndex() // This is just an empty index
  sources.foreach(index.addSourceJar)

  private val docs = new Docstrings(index)
  private val mtags = new Mtags()
  private val destinationProvider =
    new DestinationProvider(
      index,
      buffers,
      mtags,
      workspace,
      semanticdbsFallback = None
    )

  def documentation(symbol: String): ju.Optional[SymbolDocumentation] =
    docs
      .documentation(symbol)
      .asScala
      .orElse(workspaceFallback.flatMap(_.documentation(symbol).asScala))
      .asJava

  override def definition(x: String): ju.List[Location] = {
    pprint.log("In definition method in the standalone symbol search")
    val destinations = destinationProvider
      .fromSymbol(x)
      .flatMap(_.toResult)
      .map(_.locations)

    pprint.log("Seeing what we got from the standalone")
    pprint.log(destinations)
    destinations
      .orElse(workspaceFallback.map(_.definition(x)))
      .getOrElse(ju.Collections.emptyList())
  }

  def definitionSourceToplevels(sym: String): ju.List[String] =
    index
      .definition(Symbol(sym))
      .map { symDef =>
        val input = symDef.path.toInput
        dependencySourceCache.getOrElseUpdate(
          symDef.path,
          mtags.toplevels(input).asJava
        )
      }
      .orElse(workspaceFallback.map(_.definitionSourceToplevels(sym)))
      .getOrElse(ju.Collections.emptyList())

  def search(
      query: String,
      buildTargetIdentifier: String,
      visitor: SymbolSearchVisitor
  ): Result = {
    val res = classpathSearch.search(WorkspaceSymbolQuery.exact(query), visitor)
    workspaceFallback
      .map(_.search(query, buildTargetIdentifier, visitor))
      .getOrElse(res)
  }
}

object StandaloneSymbolSearch {
  def apply(
      workspace: AbsolutePath,
      buffers: Buffers,
      sources: Seq[Path],
      classpath: Seq[Path],
      isExcludedPackage: String => Boolean,
      userConfig: () => UserConfiguration
  ): StandaloneSymbolSearch = {
    val (sourcesWithExtras, classpathWithExtras) =
      addScalaAndJava(
        sources.map(AbsolutePath(_)),
        classpath.map(AbsolutePath(_)),
        userConfig().javaHome
      )

    new StandaloneSymbolSearch(
      workspace,
      classpathWithExtras,
      sourcesWithExtras,
      buffers,
      isExcludedPackage
    )
  }
  def apply(
      workspace: AbsolutePath,
      buffers: Buffers,
      isExcludedPackage: String => Boolean,
      userConfig: () => UserConfiguration
  ): StandaloneSymbolSearch = {
    val (sourcesWithExtras, classpathWithExtras) =
      addScalaAndJava(Nil, Nil, userConfig().javaHome)

    new StandaloneSymbolSearch(
      workspace,
      classpathWithExtras,
      sourcesWithExtras,
      buffers,
      isExcludedPackage
    )
  }

  /**
   * When creating the standalone check to see if the sources have Scala. If
   * not, we add the sources and the classpath. We also add in the JDK sources.
   *
   * @param sources the original sources that may be included.
   * @param classpath the current classpath.
   * @param javaHome possible JavaHome to get the JDK sources from.
   * @return (scalaSources, scalaClasspath)
   */
  private def addScalaAndJava(
      sources: Seq[AbsolutePath],
      classpath: Seq[AbsolutePath],
      javaHome: Option[String]
  ): (Seq[AbsolutePath], Seq[AbsolutePath]) = {
    val missingScala: Boolean =
      sources.filter(_.toString.contains("scala-library")).isEmpty

    (missingScala, JdkSources(javaHome)) match {
      case (true, None) =>
        val (scalaSources, scalaClasspath) = getScala(
          scala.meta.internal.mtags.BuildInfo.scalaCompilerVersion
        )
        (sources ++ scalaSources, classpath ++ scalaClasspath)
      case (true, Some(absPath)) =>
        val (scalaSources, scalaClasspath) = getScala(
          scala.meta.internal.mtags.BuildInfo.scalaCompilerVersion
        )
        (
          (scalaSources :+ absPath) ++ sources,
          classpath ++ scalaClasspath
        )
      case (false, None) => (sources, classpath)
      case (false, Some(absPath)) =>
        (sources :+ absPath, classpath)
    }
  }

  /**
   * Retrieve scala for the given version and partition sources.
   * @param scalaVersion
   * @return (scalaSources, scalaClasspath)
   */
  private def getScala(
      scalaVersion: String
  ): (Seq[AbsolutePath], Seq[AbsolutePath]) =
    Embedded
      .downloadScalaSources(scalaVersion)
      .toSeq
      .map(path => AbsolutePath(path))
      .partition(_.toString.endsWith("-sources.jar"))

}
