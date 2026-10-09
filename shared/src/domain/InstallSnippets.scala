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
