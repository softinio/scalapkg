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

/** One Maven artifact, any version. Package keys are [[PackageRef]]: the types
  * differ, so one cannot be passed where the other is expected.
  */
final case class ArtifactKey(groupId: String, artifactId: String):
  /** deps.dev's package name, `groupId:artifactId`. */
  def name: String = s"$groupId:$artifactId"

/** One version of one Maven artifact, as deps.dev and Maven Central address it.
  */
final case class ArtifactVersion(
    groupId: String,
    artifactId: String,
    version: String
):
  /** deps.dev's package name, `groupId:artifactId`. */
  def name: String = s"$groupId:$artifactId"

  def depsDevUrl: String =
    s"https://deps.dev/maven/${name.replace(":", "%3A")}/$version"

  def mavenCentralUrl: String =
    s"https://central.sonatype.com/artifact/$groupId/$artifactId/$version"

/** Outbound links for a package, shared by server pages and the drawer. */
object Links:

  def advisory(id: String): String = s"https://osv.dev/vulnerability/$id"
