package scala.meta.internal.metals

import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicBoolean

import scala.concurrent.ExecutionContext

import scala.meta.internal.bsp.BspResolvedResult
import scala.meta.internal.bsp.BspSession
import scala.meta.internal.bsp.ResolvedBloop
import scala.meta.internal.bsp.ResolvedBspOne
import scala.meta.internal.bsp.ResolvedMultiple
import scala.meta.internal.bsp.ResolvedNone
import scala.meta.internal.metals.Messages.CheckDoctor
import scala.meta.internal.metals.MetalsEnrichments._
import scala.meta.internal.metals.clients.language.MetalsLanguageClient
import scala.meta.internal.metals.config.DoctorFormat
import scala.meta.internal.troubleshoot.ProblemResolver
import scala.meta.io.AbsolutePath

import ch.epfl.scala.bsp4j.BuildTargetIdentifier

/**
 * Helps the user figure out what is mis-configured in the build through the "Run doctor" command.
 *
 * At the moment, the doctor only validates that SemanticDB is enabled for all projects.
 */
final class Doctor(
    workspace: AbsolutePath,
    buildTargets: BuildTargets,
    languageClient: MetalsLanguageClient,
    currentBuildServer: () => Option[BspSession],
    calculateNewBuildServer: () => BspResolvedResult,
    httpServer: () => Option[MetalsHttpServer],
    tables: Tables,
    clientConfig: ClientConfiguration,
    mtagsResolver: MtagsResolver,
    javaHome: () => Option[String]
)(implicit ec: ExecutionContext) {
  private val hasProblems = new AtomicBoolean(false)
  private val problemResolver =
    new ProblemResolver(
      workspace,
      mtagsResolver,
      currentBuildServer,
      javaHome
    )

  /**
   * Returns a full HTML page for the HTTP client.
   */
  def problemsHtmlPage(url: String): String = {
    val livereload = Urls.livereload(url)
    HtmlBuilder()
      .page(
        doctorTitle,
        List(livereload, HtmlBuilder.htmlCSS),
        HtmlBuilder.bodyStyle
      ) { html =>
        html.section("Build targets", buildTargetsTable)
      }
      .render
  }

  /**
   * Executes the "Run doctor" server command.
   */
  def executeRunDoctor(): Unit = {
    executeDoctor(
      ClientCommands.RunDoctor,
      server => {
        Urls.openBrowser(server.address + "/doctor")
      }
    )
  }

  def isUnsupportedBloopVersion(): Boolean =
    problemResolver.isUnsupportedBloopVersion()

  /**
   * Executes the "Reload doctor" server command.
   */
  private def executeReloadDoctor(summary: Option[String]): Unit = {
    val hasProblemsNow = summary.isDefined
    if (hasProblems.get() && !hasProblemsNow) {
      hasProblems.set(false)
      languageClient.showMessage(CheckDoctor.problemsFixed)
    }
    executeRefreshDoctor()
  }

  def executeRefreshDoctor(): Unit = {
    executeDoctor(
      ClientCommands.ReloadDoctor,
      server => {
        server.reload()
      }
    )
  }

  private def executeDoctor(
      clientCommand: ParametrizedCommand[String],
      onServer: MetalsHttpServer => Unit
  ): Unit = {
    if (
      clientConfig.isExecuteClientCommandProvider && !clientConfig.isHttpEnabled
    ) {
      val output = clientConfig.doctorFormat match {
        case DoctorFormat.Json => buildTargetsJson()
        case DoctorFormat.Html => buildTargetsHtml()
      }
      val params = clientCommand.toExecuteCommandParams(output)
      languageClient.metalsExecuteClientCommand(params)
    } else {
      httpServer() match {
        case Some(server) =>
          onServer(server)
        case None =>
          scribe.warn(
            "Unable to run doctor. Make sure `isHttpEnabled` is set to `true`."
          )
      }
    }
  }

  /**
   * Checks if there are any potential problems and if any, notifies the user.
   */
  def check(): Unit = {
    val scalaTargets = buildTargets.allScala.toList
    val javaTargets = buildTargets.allJava.toList
    val summary = problemResolver.problemMessage(scalaTargets, javaTargets)
    executeReloadDoctor(summary)
    summary match {
      case Some(problem) =>
        val notification = tables.dismissedNotifications.DoctorWarning
        if (!notification.isDismissed) {
          notification.dismiss(2, TimeUnit.MINUTES)
          import scala.meta.internal.metals.Messages.CheckDoctor
          val params = CheckDoctor.params(problem)
          hasProblems.set(true)
          languageClient.showMessageRequest(params).asScala.foreach { item =>
            if (item == CheckDoctor.moreInformation) {
              executeRunDoctor()
            } else if (item == CheckDoctor.dismissForever) {
              notification.dismissForever()
            }
          }
        }
      case None =>
        () // All OK.
    }
  }

  private def allTargetIds(): Seq[BuildTargetIdentifier] =
    buildTargets.allBuildTargetIds

  private def selectedBuildToolMessage(): Option[String] = {
    tables.buildTool.selectedBuildTool().map { value =>
      s"Build definition is coming from ${value}."
    }
  }

  private def selectedImportBuildMessage(): Option[String] = {
    import scala.concurrent.duration._
    tables.dismissedNotifications.ImportChanges.whenExpires().map {
      expiration =>
        val whenString =
          if (expiration > 1000.days.toMillis) "forever"
          else "temporarily"
        s"Build import popup on configuration changes has been dismissed $whenString"
    }
  }

  /**
   * @return Selected build server message and also whether or not the server was chosen
   *         explicity. For example, if the user is just using Bloop and never made and
   *         explicit choice to use another server and then come back to Bloop, it's
   *         not explict and won't get the option to reset the choice, since no choice
   *         exists. (Message, Explict Choice)
   */
  private def selectedBuildServerMessage(): (String, Boolean) = {
    val current = currentBuildServer().map(s => (s.main.name, s.main.version))
    val chosen = tables.buildServers.selectedServer()

    (current, chosen) match {
      case (Some((name, version)), Some(_)) =>
        (s"Build server currently being used is $name v$version.", true)
      case (Some((name, version)), None) =>
        (s"Build server currently being used is $name v$version.", false)
      case (None, _) =>
        calculateNewBuildServer() match {
          case ResolvedNone =>
            (
              "No build server found. Try to run the generate-bsp-config command.",
              false
            )
          case ResolvedBloop =>
            ("Build server currently being used is Bloop.", false)
          case ResolvedBspOne(details) =>
            (
              s"Build server currently being used is ${details.getName()}.",
              false
            )
          case ResolvedMultiple(_, _) =>
            (
              "Multiple build servers found for your workspace. Attempt to connect to choose your desired server.",
              false
            )
        }
    }
  }

  private def buildTargetsHtml(): String = {
    new HtmlBuilder()
      .element("h1")(_.text(doctorTitle))
      .call(buildTargetsTable)
      .render
  }

  private def buildTargetsJson(): String = {
    val targetIds = allTargetIds()
    val buildToolHeading = selectedBuildToolMessage()
    val (buildServerHeading, _) = selectedBuildServerMessage()
    val importBuildHeading = selectedImportBuildMessage()
    val heading =
      List(
        buildToolHeading,
        Some(buildServerHeading),
        importBuildHeading,
        Some(doctorHeading)
      ).flatten
        .mkString("\n\n")

    val results = if (targetIds.isEmpty) {
      DoctorResults(
        doctorTitle,
        heading,
        Some(
          List(
            DoctorMessage(
              noBuildTargetsTitle,
              List(noBuildTargetRecOne, noBuildTargetRecTwo)
            )
          )
        ),
        None
      ).toJson
    } else {
      val targetResults = targetIds
        .flatMap(extractTargetInfo)
        .sortBy(f => (f.baseDirectory, f.name, f.dataKind))
      DoctorResults(doctorTitle, heading, None, Some(targetResults)).toJson
    }
    ujson.write(results)
  }

  private def resetChoiceCommand(choice: String): String = {
    val param = s"""["$choice"]"""
    s"command:metals.reset-choice?${URLEncoder.encode(param, StandardCharsets.UTF_8.name())}"
  }

  private def buildTargetsTable(html: HtmlBuilder): Unit = {
    selectedBuildToolMessage().foreach { msg =>
      html.element("p")(
        _.text(msg)
          .optionally(!clientConfig.isHttpEnabled)(
            _.text(" (")
              .link(resetChoiceCommand(PopupChoiceReset.BuildTool), "Reset")
              .text(")")
          )
      )
    }

    selectedImportBuildMessage().foreach { msg =>
      html.element("p")(
        _.text(msg)
          .optionally(!clientConfig.isHttpEnabled)(
            _.text(" (")
              .link(resetChoiceCommand(PopupChoiceReset.BuildImport), "Reset")
              .text(")")
          )
      )
    }

    val (message, explicitChoice) = selectedBuildServerMessage()

    if (explicitChoice) {
      html.element("p")(
        _.text(message)
          .optionally(!clientConfig.isHttpEnabled)(
            _.text(" (")
              .link(resetChoiceCommand(PopupChoiceReset.BuildServer), "Reset")
              .text(")")
          )
      )
    } else {
      html.element("p")(
        _.text(message)
      )
    }

    html
      .element("p")(
        _.text(doctorHeading)
      )

    val targetIds = allTargetIds()
    if (targetIds.isEmpty) {
      html
        .element("p")(
          _.text(noBuildTargetsTitle)
            .element("ul")(
              _.element("li")(
                _.text(noBuildTargetRecOne)
              ).element("li")(
                _.text(noBuildTargetRecTwo)
              )
            )
        )
    } else {
      val allTargetsInfo = targetIds.flatMap(extractTargetInfo)
      html
        .element("table")(
          _.element("thead")(
            _.element("tr")(
              _.element("th")(_.text("Build target"))
                .element("th")(_.text("Type"))
                .element("th")(_.text("Diagnostics"))
                .element("th")(_.text("Interactive"))
                .element("th")(_.text("Semanticdb"))
                .element("th")(_.text("Debugging"))
                .element("th")(_.text("Java support"))
                .element("th")(_.text("Recommendation"))
            )
          ).element("tbody")(html => buildTargetRows(html, allTargetsInfo))
        )

      // Additional explanations
      explanation(
        html,
        title = "Diagnostics:",
        correctMessage =
          s"${Icons.unicode.check} - diagnostics correctly being reported by the build server",
        incorrectMessage =
          s"${Icons.unicode.alert} - only syntactic errors being reported",
        show = allTargetsInfo.exists(_.diagnosticsStatus.isCorrect == false)
      )

      explanation(
        html,
        title = "Interactive features (completions, hover):",
        correctMessage = s"${Icons.unicode.check} - supported Scala version",
        incorrectMessage =
          s"${Icons.unicode.error} - unsupported Scala version",
        show = allTargetsInfo.exists(_.interactiveStatus.isCorrect == false)
      )

      explanation(
        html,
        title =
          "Semanticdb features (references, renames, go to implementation):",
        correctMessage =
          s"${Icons.unicode.check} - build tool automatically creating needed semanticdb files",
        incorrectMessage =
          s"${Icons.unicode.error} - semanticdb not being produced",
        show = allTargetsInfo.exists(_.indexesStatus.isCorrect == false)
      )

      explanation(
        html,
        title = "Debugging (run/test, breakpoints, evaluation):",
        correctMessage =
          s"${Icons.unicode.check} - users can run or test their code with debugging capabilities",
        incorrectMessage =
          s"${Icons.unicode.error} - the tool does not support debugging in this target",
        show = allTargetsInfo.exists(_.debuggingStatus.isCorrect == false)
      )

      explanation(
        html,
        title = "Java Support:",
        correctMessage =
          s"${Icons.unicode.check} - working non-interactive features (references, rename etc.)",
        incorrectMessage =
          s"${Icons.unicode.error} - missing semanticdb plugin, might not be added automatically by the build server",
        show = allTargetsInfo.exists(_.javaStatus.isCorrect == false)
      )
    }
  }

  private def explanation(
      html: HtmlBuilder,
      title: String,
      correctMessage: String,
      incorrectMessage: String,
      show: Boolean
  ) = {
    html.element("div")(
      _.element("p")(_.text(title)).element("ul") { ul =>
        ul.element("li")(_.text(correctMessage))
        if (show)
          ul.element("li")(_.text(incorrectMessage))
      }
    )

  }

  private def buildTargetRows(
      html: HtmlBuilder,
      targetIds: Seq[DoctorTargetInfo]
  ): Unit = {
    targetIds
      .sortBy(f => (f.baseDirectory, f.name, f.dataKind))
      .foreach { targetInfo =>
        val center = "style='text-align: center'"
        html.element("tr")(
          _.element("td")(_.text(targetInfo.name))
            .element("td")(_.text(targetInfo.targetType))
            .element("td", center)(
              _.text(targetInfo.diagnosticsStatus.explanation)
            )
            .element("td", center)(
              _.text(targetInfo.interactiveStatus.explanation)
            )
            .element("td", center)(_.text(targetInfo.indexesStatus.explanation))
            .element("td", center)(
              _.text(targetInfo.debuggingStatus.explanation)
            )
            .element("td", center)(_.text(targetInfo.javaStatus.explanation))
            .element("td")(_.raw(targetInfo.recommenedFix))
        )
      }
  }

  private def extractTargetInfo(
      targetId: BuildTargetIdentifier
  ): List[DoctorTargetInfo] = {
    val javaTarget = buildTargets.javaTarget(targetId)
    buildTargets
      .scalaTarget(targetId)
      .map(st => extractScalaTargetInfo(st, javaTarget))
      .toList
  }

  private def extractScalaTargetInfo(
      target: ScalaTarget,
      javaTarget: Option[JavaTarget]
  ) = {
    val scalaVersion = target.scalaVersion
    val interactive =
      if (mtagsResolver.isSupportedScalaVersion(scalaVersion)) {
        DoctorStatus(Icons.unicode.check, isCorrect = true)
      } else {
        DoctorStatus(Icons.unicode.error, isCorrect = false)
      }
    val isSemanticdbNeeded = !target.isSemanticdbEnabled
    val indexes =
      if (isSemanticdbNeeded) {
        DoctorStatus(Icons.unicode.error, isCorrect = false)
      } else {
        DoctorStatus(Icons.unicode.check, isCorrect = true)
      }
    val recommendedFix = problemResolver.recommendation(target)
    val (targetType, diagnostics) =
      target.sbtVersion match {
        case Some(sbt) =>
          (s"sbt $sbt", DoctorStatus(Icons.unicode.alert, isCorrect = false))
        case None =>
          (
            s"Scala $scalaVersion",
            DoctorStatus(Icons.unicode.check, isCorrect = true)
          )
      }
    val (javaSupport, javaRecommendation) = javaTarget match {
      case Some(target) if target.isSemanticdbEnabled =>
        (DoctorStatus(Icons.unicode.check, isCorrect = true), None)
      case Some(target) =>
        (
          DoctorStatus(Icons.unicode.alert, isCorrect = false),
          problemResolver.recommendation(target)
        )
      case None => (DoctorStatus(Icons.unicode.alert, isCorrect = false), None)
    }

    val canRun = target.info.getCapabilities().getCanRun()
    val canTest = target.info.getCapabilities().getCanTest()
    val debugging =
      if (canRun && canTest && !target.isSbt)
        DoctorStatus(Icons.unicode.check, isCorrect = true)
      else DoctorStatus(Icons.unicode.error, isCorrect = false)
    val sbtRecommendation =
      if (target.isSbt)
        Some("Diagnostics and debugging for sbt are not supported currently.")
      else None
    DoctorTargetInfo(
      target.displayName,
      target.dataKind,
      target.baseDirectory,
      targetType,
      diagnostics,
      interactive,
      indexes,
      debugging,
      javaSupport,
      recommendedFix
        .orElse(javaRecommendation)
        .orElse(sbtRecommendation)
        .getOrElse("")
    )
  }

  private val doctorTitle = "Metals Doctor"
  private val doctorHeading =
    "These are the installed build targets for this workspace. " +
      "One build target corresponds to one classpath. For example, normally one sbt project maps to " +
      "two build targets: main and test."
  private val noBuildTargetsTitle =
    s"${Icons.unicode.alert} No build targets were detected in this workspace so most functionality won't work."
  private val noBuildTargetRecOne =
    s"Make sure the workspace directory '$workspace' matches the root of your build."
  private val noBuildTargetRecTwo =
    "Try removing the directories .metals/ and .bloop/, then restart metals And import the build again."
}
