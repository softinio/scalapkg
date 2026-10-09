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

/** Scala binary versions an artifact can be published for. */
object ScalaVersions:
  /** Newest first: the order facets and badges are shown in. */
  val supported: List[String] = List("3", "2.13", "2.12", "2.11", "2.10")

  def isSupported(v: String): Boolean = supported.contains(v)

/** Where an artifact runs. The version is the platform's binary version as it
  * appears in the artifactId (`1` in `_sjs1_3`, `0.5` in `_native0.5_3`).
  */
enum Platform derives CanEqual:
  case Jvm
  case ScalaJs(version: String)
  case ScalaNative(version: String)
  case SbtPlugin(version: String)
  case MillPlugin(version: String)

  /** Facet value used in URLs and SQL. */
  def facet: String = this match
    case Jvm            => "jvm"
    case ScalaJs(_)     => "js"
    case ScalaNative(_) => "native"
    case SbtPlugin(_)   => "sbt"
    case MillPlugin(_)  => "mill"

  def label: String = this match
    case Jvm            => "JVM"
    case ScalaJs(v)     => s"Scala.js $v"
    case ScalaNative(v) => s"Scala Native $v"
    case SbtPlugin(v)   => s"sbt $v"
    case MillPlugin(v)  => s"Mill $v"

object Platform:
  val facets: List[String] = List("jvm", "js", "native", "sbt", "mill")

  def facetLabel(facet: String): String = facet match
    case "jvm"    => "JVM"
    case "js"     => "Scala.js"
    case "native" => "Scala Native"
    case "sbt"    => "sbt plugin"
    case "mill"   => "Mill plugin"
    case other    => other

/** One platform + Scala version combination, e.g. Scala.js 1 on Scala 3. `key`
  * is the compact form stored in the database and sent to the browser.
  */
final case class Target(platform: Platform, scalaVersion: String):
  def key: String = platform match
    case Platform.Jvm            => s"jvm:$scalaVersion"
    case Platform.ScalaJs(v)     => s"sjs$v:$scalaVersion"
    case Platform.ScalaNative(v) => s"native$v:$scalaVersion"
    case Platform.SbtPlugin(v)   => s"sbt$v:$scalaVersion"
    case Platform.MillPlugin(v)  => s"mill$v:$scalaVersion"

  def label: String = platform match
    case Platform.Jvm => s"Scala $scalaVersion"
    case p            => s"${p.label} · Scala $scalaVersion"

  /** The Maven artifactId for library `name` on this target. */
  def artifactId(name: String): String = platform match
    case Platform.Jvm               => s"${name}_$scalaVersion"
    case Platform.ScalaJs(v)        => s"${name}_sjs${v}_$scalaVersion"
    case Platform.ScalaNative(v)    => s"${name}_native${v}_$scalaVersion"
    case Platform.MillPlugin(v)     => s"${name}_mill${v}_$scalaVersion"
    case Platform.SbtPlugin("1")    => s"${name}_${scalaVersion}_1.0"
    case Platform.SbtPlugin("0.13") => s"${name}_${scalaVersion}_0.13"
    case Platform.SbtPlugin(v)      => s"${name}_sbt${v}_$scalaVersion"

object Target:
  def parseKey(key: String): Option[Target] =
    key.split(':') match
      case Array(p, sv) if ScalaVersions.isSupported(sv) =>
        val platform = p match
          case "jvm"                     => Some(Platform.Jvm)
          case s"sjs$v" if v.nonEmpty    => Some(Platform.ScalaJs(v))
          case s"native$v" if v.nonEmpty => Some(Platform.ScalaNative(v))
          case s"sbt$v" if v.nonEmpty    => Some(Platform.SbtPlugin(v))
          case s"mill$v" if v.nonEmpty   => Some(Platform.MillPlugin(v))
          case _                         => None
        platform.map(Target(_, sv))
      case _ => None

  given Ordering[Target] = Ordering.by { t =>
    val platformRank = t.platform match
      case Platform.Jvm            => 0
      case Platform.ScalaJs(_)     => 1
      case Platform.ScalaNative(_) => 2
      case Platform.SbtPlugin(_)   => 3
      case Platform.MillPlugin(_)  => 4
    (platformRank, ScalaVersions.supported.indexOf(t.scalaVersion))
  }

/** An artifactId split into the library name and its target. */
final case class ParsedArtifact(name: String, target: Target)

/** Parses Scala artifactIds such as `cats-core_3`, `fs2-io_sjs1_2.13`,
  * `sbt-scalafmt_2.12_1.0` or `mill-scalafix_mill1_3`.
  *
  * The artifactId is split on `_` and read from the right: a Scala version,
  * optionally preceded by a platform segment (`sjs1`, `native0.5`, `mill1`,
  * `sbt2`), or a classic sbt plugin's `<scala>_<sbt>` pair. What is left is the
  * library name.
  */
object ArtifactIds:

  def parse(artifactId: String): Option[ParsedArtifact] =
    val target = artifactId.split('_').toList.reverse match
      case sbt :: scala :: name if isClassicSbt(scala, sbt) =>
        val v = if sbt == "1.0" then "1" else sbt
        Some(name -> Target(Platform.SbtPlugin(v), scala))
      case scala :: PlatformSegment(p) :: name
          if ScalaVersions.isSupported(scala) =>
        Some(name -> Target(p, scala))
      case scala :: name if ScalaVersions.isSupported(scala) =>
        Some(name -> Target(Platform.Jvm, scala))
      case _ => None
    target.collect {
      case (reversedName, t) if isValidName(reversedName) =>
        ParsedArtifact(reversedName.reverse.mkString("_"), t)
    }

  private def isClassicSbt(scala: String, sbt: String): Boolean =
    (sbt == "1.0" || sbt == "0.13") &&
      (scala == "2.12" || scala == "2.11" || scala == "2.10")

  private object PlatformSegment:
    def unapply(segment: String): Option[Platform] = segment match
      case s"sjs$v" if v == "1" || v == "0.6" => Some(Platform.ScalaJs(v))
      case s"native0.$m" if "345".contains(m) && m.length == 1 =>
        Some(Platform.ScalaNative(s"0.$m"))
      case s"mill$v" if isMillVersion(v) => Some(Platform.MillPlugin(v))
      case s"sbt$v" if isSbtVersion(v)   => Some(Platform.SbtPlugin(v))
      case _                             => None

  private def isNumber(s: String) = s.nonEmpty && s.forall(_.isDigit)

  private def isMillVersion(v: String): Boolean = v.split('.') match
    case Array(major)        => isNumber(major)
    case Array(major, minor) => isNumber(major) && isNumber(minor)
    case _                   => false

  // only sbt 2 names plugins with a platform segment (`_sbt2_3`); sbt 1 and
  // 0.13 plugins use the classic `_2.12_1.0` pair, so `_sbt1_2.12` is no
  // known convention and is rejected rather than guessed
  private def isSbtVersion(v: String): Boolean =
    v == "2" || (v.startsWith("2.0.0-") && v.length > "2.0.0-".length)

  /** `foo_sjs2_3` must not become a library called `foo_sjs2`: a name whose
    * last segment looks like a version or platform means the artifact targets
    * something unsupported.
    */
  private def looksLikeSuffix(segment: String): Boolean =
    val version = List("sjs", "native", "mill", "sbt")
      .find(segment.startsWith)
      .fold(segment)(p => segment.drop(p.length))
    version.headOption.exists(_.isDigit) &&
    version.forall(c => c.isDigit || c == '.')

  private def isValidName(reversed: List[String]): Boolean = reversed match
    case last :: _ =>
      reversed.forall(seg =>
        seg.nonEmpty && seg.forall(c => c.isLetterOrDigit || "-.".contains(c))
      ) && !looksLikeSuffix(last)
    case Nil => false
