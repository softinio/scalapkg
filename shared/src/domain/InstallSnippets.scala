/*
 * Copyright 2026 Salar Rahmanian
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.softinio.scalapkg.domain

import io.circe.Codec

import scala.math.Ordering.Implicits.seqOrdering

/** One tab of install lines: `tool` is `sbt`, `Mill` or `scala-cli`. */
final case class Snippet(tool: String, code: String) derives Codec.AsObject

/** Copy-paste dependency lines for sbt, Mill and scala-cli. */
object InstallSnippets:

  /** A plugin gets only the line that adds it; a library gets a JVM line, a
    * Scala.js/Native line or both. Tools with no line are left out.
    */
  def render(
      groupId: String,
      name: String,
      version: String,
      targets: List[Target]
  ): List[Snippet] =
    val platforms = targets.map(_.platform)
    val g = groupId
    val sbtPlugin = platforms.collectFirst { case p: Platform.SbtPlugin => p }
    val millPlugin = platforms.collectFirst { case p: Platform.MillPlugin => p }
    val hasJvm = platforms.contains(Platform.Jvm)
    val hasCross = platforms.exists {
      case Platform.ScalaJs(_) | Platform.ScalaNative(_) => true
      case _                                             => false
    }

    if sbtPlugin.isDefined then
      List(Snippet("sbt", s"""addSbtPlugin("$g" % "$name" % "$version")"""))
    else if millPlugin.isDefined then
      List(Snippet("Mill", s"//| mvnDeps:\n//| - $g::$name::$version"))
    else
      val sbt =
        Option.when(hasJvm)(
          s"""libraryDependencies += "$g" %% "$name" % "$version""""
        ) ++ Option.when(hasCross)(
          s"""libraryDependencies += "$g" %%% "$name" % "$version""""
        )
      val millLines =
        Option.when(hasJvm)(s"""mvn"$g::$name:$version"""") ++
          Option.when(hasCross)(s"""mvn"$g::$name::$version"""")
      val cli =
        Option.when(hasJvm)(s"//> using dep $g::$name:$version") ++
          Option.when(hasCross)(s"//> using dep $g::$name::$version")
      List(
        Snippet("sbt", sbt.mkString("\n")),
        Snippet("Mill", millLines.mkString("\n")),
        Snippet("scala-cli", cli.mkString("\n"))
      ).filter(_.code.nonEmpty)

  /** Lines for one build, from [[forScala]]: the targets they use with the
    * version each names, and a note when they are not a plain match.
    */
  final case class Matched(
      snippets: List[Snippet],
      versions: List[(Target, String)],
      note: Option[String]
  )

  private val tools = List("sbt", "Mill", "scala-cli")

  /** Install lines for a build on Scala `scala`, naming the newest version
    * published for it rather than the package's latest, which may have dropped
    * that Scala version. `versions` holds the newest version of each of the
    * package's targets; `platform` (a `Platform.facet`) keeps one platform's
    * lines. A Scala 3 build gets a 2.13-only JVM library through `for3Use2_13`.
    * Left says why there is no line.
    */
  def forScala(
      p: PackageSummary,
      versions: Map[Target, String],
      scala: String,
      platform: Option[String]
  ): Either[String, Matched] =
    val g = p.groupId
    val n = p.name
    val libs = versions.filter((t, _) => platform.forall(_ == t.platform.facet))
    // per platform, the newest platform version: sjs1 rather than sjs0.6
    def on(sv: String): List[(Target, String)] =
      libs.toList
        .filter(_._1.scalaVersion == sv)
        .groupBy(_._1.platform.facet)
        .values
        .map(_.maxBy(t => platformVersion(t._1.platform)))
        .toList
        .sortBy(_._1)
    val plugin = p.parsedTargets.exists(_.platform match
      case Platform.SbtPlugin(_) | Platform.MillPlugin(_) => true
      case _                                              => false)
    lazy val exact = on(scala)
    lazy val jvm213 = on("2.13").find(_._1.platform == Platform.Jvm)
    if plugin then
      Right(
        Matched(
          p.install,
          p.parsedTargets.map(_ -> p.latestVersion),
          Some("A build plugin: the build's own Scala version does not matter.")
        )
      )
    else if exact.nonEmpty then Right(Matched(merged(g, n, exact), exact, None))
    else if scala == "3" && jvm213.isDefined then
      val (t, v) = jvm213.get
      Right(
        Matched(
          List(
            Snippet(
              "sbt",
              s"""libraryDependencies += ("$g" %% "$n" % "$v").cross(CrossVersion.for3Use2_13)"""
            ),
            Snippet("Mill", s"""mvn"$g:${n}_2.13:$v""""),
            Snippet("scala-cli", s"//> using dep $g:${n}_2.13:$v")
          ),
          List(t -> v),
          Some(
            s"$g:$n is not published for Scala 3; these lines use its Scala 2.13 artifact, which a Scala 3 build can use unless the library relies on Scala 2 macros."
          )
        )
      )
    else
      val published = libs.keys.toList.sorted.map(_.label).distinct
      val where = platform.fold("")(f => s" on ${Platform.facetLabel(f)}")
      Left(
        if published.isEmpty then s"$g:$n is not published$where"
        else
          s"$g:$n is not published for Scala $scala$where; it is for ${published.mkString(", ")}"
      )

  /** The lines for `targets`, one per version they name, merged per tool. */
  private def merged(
      g: String,
      n: String,
      targets: List[(Target, String)]
  ): List[Snippet] =
    val byVersion = targets.groupMap(_._2)(_._1).toList.sortBy(_._2.min)
    val codes = byVersion
      .flatMap((v, ts) => render(g, n, v, ts))
      .groupMap(_.tool)(_.code)
    tools.flatMap(t => codes.get(t).map(cs => Snippet(t, cs.mkString("\n"))))

  // sjs/native binary versions, compared as numbers
  private def platformVersion(p: Platform): List[Int] =
    val v = p match
      case Platform.ScalaJs(v)     => v
      case Platform.ScalaNative(v) => v
      case _                       => ""
    v.split('.').toList.flatMap(_.toIntOption)
