package scala.meta.internal.tvp

import java.nio.file.Paths
import java.util.concurrent.ScheduledFuture

import scala.collection.concurrent.TrieMap

import scala.meta.Dialect
import scala.meta.dialects
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.MetalsServerConfig.MetalsClientType
import scala.meta.internal.metals.ReportContext
import scala.meta.internal.metals._
import scala.meta.internal.metals.clients.language.MetalsLanguageClient
import scala.meta.internal.mtags.GlobalSymbolIndex
import scala.meta.internal.mtags.Mtags
import scala.meta.internal.mtags.Symbol
import scala.meta.internal.parsing.Trees
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolOccurrence
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j.BuildTarget
import ch.epfl.scala.bsp4j.BuildTargetIdentifier
import org.eclipse.{lsp4j => l}

class MetalsTreeViewProvider(
    getFolderTreeViewProviders: () => List[FolderTreeViewProvider],
    languageClient: MetalsLanguageClient,
)(implicit rc: ReportContext)
    extends TreeViewProvider {
  private val ticks =
    TrieMap.empty[String, ScheduledFuture[_]]

  override def init(): Unit = {
    languageClient.metalsTreeViewDidChange(
      TreeViewDidChangeParams(
        Array(
          TreeViewNode.empty(Project),
          TreeViewNode.empty(Build),
        )
      )
    )
  }

  override def reset(): Unit = getFolderTreeViewProviders().foreach(_.reset())

  def echoCommand(command: BaseCommand, icon: String): TreeViewNode =
    TreeViewNode(
      viewId = "help",
      nodeUri = s"help:${command.id}",
      label = command.title,
      command = MetalsCommand(
        command.title,
        ClientCommands.EchoCommand.id,
        command.description,
        Array(command.id: AnyRef),
      ),
      icon = icon,
      tooltip = command.description,
    )

  override def children(
      params: TreeViewChildrenParams
  ): MetalsTreeViewChildrenResult = {
    val folderTreeViewProviders = getFolderTreeViewProviders()
    val children: Array[TreeViewNode] = params.viewId match {
      case Help =>
        Array(
          echoCommand(ServerCommands.RunDoctor, "bug"),
          echoCommand(ServerCommands.GotoLog, "bug"),
          echoCommand(ServerCommands.ReadVscodeDocumentation, "book"),
          echoCommand(ServerCommands.ReadBloopDocumentation, "book"),
          echoCommand(ServerCommands.ChatOnDiscord, "discord"),
          echoCommand(ServerCommands.OpenIssue, "issue-opened"),
          echoCommand(ServerCommands.OpenFeatureRequest, "github"),
          echoCommand(ServerCommands.MetalsGithub, "github"),
          echoCommand(ServerCommands.BloopGithub, "github"),
          echoCommand(ServerCommands.ScalametaTwitter, "twitter"),
        )
      case Project =>
        val showFolderName = folderTreeViewProviders.length > 1
        folderTreeViewProviders
          .map(_.getProjectRoot(Option(params.nodeUri), showFolderName))
          .flatten
          .toArray
      case Build =>
        Option(params.nodeUri) match {
          case None =>
            val base = Array(
              TreeViewNode.fromCommand(ServerCommands.ImportBuild, "sync"),
              TreeViewNode
                .fromCommand(ServerCommands.NewScalaProject, "empty-window"),
              TreeViewNode
                .fromCommand(ServerCommands.ConnectBuildServer, "connect"),
              TreeViewNode
                .fromCommand(ServerCommands.BspSwitch, "discard"),
              TreeViewNode
                .fromCommand(ServerCommands.CascadeCompile, "cascade"),
              TreeViewNode.fromCommand(ServerCommands.CancelCompile, "cancel"),
              TreeViewNode.fromCommand(ServerCommands.CleanCompile, "clean"),
              TreeViewNode
                .fromCommand(ServerCommands.RestartBuildServer, "debug-stop"),
              TreeViewNode
                .fromCommand(ServerCommands.ResetWorkspace, "clean-all"),
              TreeViewNode
                .fromCommand(
                  ServerCommands.ResetNotifications,
                  "notifications-clear",
                ),
              TreeViewNode
                .fromCommand(
                  ServerCommands.AnalyzeStacktrace,
                  "bug",
                ),
            )
            if (
              MetalsServerConfig.metalsClientType.contains(
                MetalsClientType.vscode
              )
            ) {
              base :+
                TreeViewNode
                  .fromCommand(
                    ClientCommands.FindTextInDependencyJars,
                    "search",
                  )
            } else base
          case _ =>
            Array()
        }
      case _ => Array.empty
    }
    MetalsTreeViewChildrenResult(children)
  }
  override def reveal(
      path: AbsolutePath,
      pos: l.Position,
  ): Option[TreeViewNodeRevealResult] = {
    def dialectFromWorkspace =
      getFolderTreeViewProviders().iterator
        .map(_.dialectOf(path))
        .collectFirst { case Some(dialect) =>
          dialect
        }
        .getOrElse(dialects.Scala213)
    val dialect =
      if (path.isJarFileSystem) {
        path.jarPath
          .flatMap(p => ScalaVersions.scalaBinaryVersionFromJarName(p.filename))
          .map(ScalaVersions.dialectForScalaVersion(_, includeSource3 = true))
          .getOrElse(dialects.Scala3)
      } else dialectFromWorkspace
    val input = path.toInput
    val occurrences =
      Mtags
        .allToplevels(
          input,
          dialect,
        )
        .occurrences
        .filterNot(_.symbol.isPackage)
    if (occurrences.isEmpty) None
    else {
      val closestSymbol = occurrences.minBy { occ =>
        val startLine = occ.range.fold(Int.MaxValue)(_.startLine)
        val distance = math.abs(pos.getLine - startLine)
        val isLeading = pos.getLine() >= startLine
        (!isLeading, distance)
      }
      val result =
        getFolderTreeViewProviders().foldLeft(Option.empty[List[String]]) {
          case (None, treeViewProvider) =>
            treeViewProvider.revealResult(path, closestSymbol)
          case (Some(value), _) => Some(value)
        }
      result.map { uriChain =>
        uriChain.foreach { uri =>
          // Cache results
          children(TreeViewChildrenParams(Project, uri))
        }
        TreeViewNodeRevealResult(Project, uriChain.toArray)
      }
    }
  }

  override def onCollapseDidChange(
      params: TreeViewNodeCollapseDidChangeParams
  ): Unit = getFolderTreeViewProviders().foreach(_.onCollapseDidChange(params))

  override def parent(
      params: TreeViewParentParams
  ): TreeViewParentResult =
    params.viewId match {
      case Project =>
        getFolderTreeViewProviders()
          .map(_.parent(params.nodeUri))
          .collectFirst { case Some(value) =>
            value
          }
          .getOrElse(TreeViewParentResult(null))
      case _ => TreeViewParentResult(null)
    }

  override def onVisibilityDidChange(
      params: TreeViewVisibilityDidChangeParams
  ): Unit = {
    val trees = getFolderTreeViewProviders()
    trees.foreach {
      _.setVisible(params.viewId, params.visible)
    }
    if (params.visible) {
      params.viewId match {
        case TreeViewProvider.Project =>
          val toUpdate = trees.map(_.flushPendingProjectUpdates).collect {
            case Some(value) => value
          }
          if (toUpdate.nonEmpty) {
            languageClient.metalsTreeViewDidChange(
              TreeViewDidChangeParams(toUpdate.flatten.toArray)
            )
          }
        case _ =>
      }
    } else {
      ticks.remove(params.viewId).foreach(_.cancel(false))
    }
  }

}

class FolderTreeViewProvider(
    folder: Folder,
    buildTargets: BuildTargets,
    definitionIndex: GlobalSymbolIndex,
    userConfig: () => UserConfiguration,
    scalaVersionSelector: ScalaVersionSelector,
    languageClient: MetalsLanguageClient,
    clientConfig: ClientConfiguration,
    trees: Trees,
    buffers: Buffers,
)(implicit context: ReportContext) {
  val classpath = new IndexedSymbols(
    isStatisticsEnabled = clientConfig.initialConfig.statistics.isTreeView,
    trees,
    buffers,
    buildTargets,
  )

  def dialectOf(path: AbsolutePath): Option[Dialect] =
    scalaVersionSelector.dialectFromBuildTarget(path)
  private def maybeUsedJdkVersion =
    JdkSources.defaultJavaHome(userConfig().javaHome).headOption.flatMap {
      path =>
        JdkVersion.fromReleaseFileString(path)
    }
  private val isVisible = TrieMap.empty[String, Boolean].withDefaultValue(false)
  private val isCollapsedTarget = TrieMap.empty[BuildTargetIdentifier, Boolean]
  private val pendingProjectUpdates =
    ConcurrentHashSet.empty[BuildTargetIdentifier]
  val libraries = new ClasspathTreeView[AbsolutePath, AbsolutePath](
    definitionIndex = definitionIndex,
    viewId = TreeViewProvider.Project,
    schemeId = s"libraries",
    title = s"Libraries",
    folder = folder,
    id = identity,
    encode = _.toURI.toString(),
    decode = _.toAbsolutePath(followSymlink = false),
    valueTitle = path => {
      if (path.filename == JdkSources.zipFileName) {
        maybeUsedJdkVersion
          .map(ver => s"jdk-${ver}-sources")
          .getOrElse("jdk-sources")
      } else
        path.filename
    },
    valueTooltip = _.toString,
    toplevels = () => buildTargets.allSourceJars.filter(_.exists),
    loadSymbols = (path, symbol) => {
      val dialect = ScalaVersions.dialectForDependencyJar(path, buildTargets)
      classpath.jarSymbols(path, symbol, dialect)
    },
    toplevelIcon = "package",
  )

  val projects = new ClasspathTreeView[BuildTarget, BuildTargetIdentifier](
    definitionIndex = definitionIndex,
    viewId = TreeViewProvider.Project,
    schemeId = s"projects",
    title = s"Projects",
    folder = folder,
    id = _.getId(),
    encode = _.getUri(),
    decode = uri => new BuildTargetIdentifier(uri),
    valueTitle = _.getName(),
    valueTooltip = _.baseDirectory,
    toplevels = { () =>
      buildTargets.all.filter(target =>
        buildTargets.buildTargetSources(target.getId()).nonEmpty
      )
    },
    loadSymbols = { (id, symbol) =>
      val tops = for {
        source <- buildTargets.buildTargetSources(id).iterator
      } yield classpath.workspaceSymbols(source, symbol)
      tops.flatten
    },
    toplevelIcon = "target",
  )

  def setVisible(viewId: String, visibility: Boolean): Unit = {
    isVisible(viewId) = visibility
  }

  def flushPendingProjectUpdates(): Option[Array[TreeViewNode]] = {
    val toUpdate = pendingProjectUpdates.asScala.iterator
      .filter { id =>
        !isCollapsedTarget.getOrElse(id, true) &&
        isVisible(TreeViewProvider.Project)
      }
      .flatMap(buildTargets.info)
      .toArray
    if (toUpdate.nonEmpty) {
      val nodes = toUpdate.map { target =>
        projects
          .toplevelNode(target)
          .copy(collapseState = MetalsTreeItemCollapseState.expanded)
      }
      Some(nodes)
    } else {
      None
    }
  }

  def onWorkspaceFileDidChange(
      path: AbsolutePath
  ): Unit = {
    classpath.onChange(path)
    buildTargets.inverseSources(path).foreach { id =>
      if (isCollapsedTarget.getOrElse(id, false) == false) {
        pendingProjectUpdates.add(id)
      }
    }
    flushPendingProjectUpdates() match {
      case Some(toUpdate) =>
        languageClient.metalsTreeViewDidChange(
          TreeViewDidChangeParams(toUpdate)
        )
      case None =>
    }
  }

  def onCollapseDidChange(
      params: TreeViewNodeCollapseDidChangeParams
  ): Unit = {
    if (projects.matches(params.nodeUri)) {
      val uri = projects.fromUri(params.nodeUri)
      if (uri.isRoot) {
        isCollapsedTarget(uri.key) = params.collapsed
      }
    }
  }

  def parent(
      uri: String
  ): Option[TreeViewParentResult] =
    if (libraries.matches(uri)) {
      Some(TreeViewParentResult(libraries.parent(uri).orNull))
    } else if (projects.matches(uri)) {
      Some(TreeViewParentResult(projects.parent(uri).orNull))
    } else {
      None
    }

  def getProjectRoot(
      nodeUri: Option[String],
      showFolderName: Boolean,
  ): Array[TreeViewNode] =
    nodeUri match {
      case None if buildTargets.all.nonEmpty =>
        Array(
          projects.root(showFolderName, "project"),
          libraries.root(showFolderName, "library"),
        )
      case Some(uri) =>
        if (libraries.matches(uri)) {
          libraries.children(uri)
        } else if (projects.matches(uri)) {
          projects.children(uri)
        } else {
          Array.empty
        }
      case _ => Array.empty
    }

  def revealResult(
      path: AbsolutePath,
      closestSymbol: SymbolOccurrence,
  ): Option[List[String]] = {
    if (path.isDependencySource(folder.path) || path.isJarFileSystem) {
      val closestToplevel = Symbol(closestSymbol.symbol).toplevel
      def pathJar = AbsolutePath(
        Paths.get(path.toNIO.getFileSystem().toString())
      ).dealias
      def jdkSources = JdkSources(userConfig().javaHome).toOption
        .collect {
          case sources
              if sources == pathJar || !path.isJarFileSystem &&
                path.isSrcZipInReadonlyDirectory(folder.path) =>
            libraries.toUri(sources, closestToplevel.value).parentChain
        }

      val result = buildTargets
        .inferBuildTarget(List(closestToplevel))
        .flatMap { inferred =>
          inferred.sourceJar.map { sourceJar =>
            libraries.toUri(sourceJar, inferred.symbol).parentChain
          }
        }
      result.orElse(jdkSources)
    } else {
      buildTargets
        .inverseSources(path)
        .map(id => projects.toUri(id, closestSymbol.symbol).parentChain)
    }
  }

  def reset(): Unit = {
    classpath.reset()
  }
}
