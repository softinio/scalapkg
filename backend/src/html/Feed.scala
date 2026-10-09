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

package com.softinio.scalapkg.html

import com.softinio.scalapkg.domain.Release

/** Atom feed of new releases. */
object Feed:

  private def esc(s: String): String =
    s.replace("&", "&amp;")
      .replace("<", "&lt;")
      .replace(">", "&gt;")
      .replace("\"", "&quot;")

  def atom(baseUrl: String, releases: List[Release], updated: String): String =
    val entries = releases.map { r =>
      val url = s"$baseUrl${r.ref.path}"
      val summary = r.project.flatMap(_.blurb).getOrElse("")
      s"""  <entry>
         |    <id>$url@${esc(r.version)}</id>
         |    <title>${esc(s"${r.groupId}:${r.name} ${r.version}")}</title>
         |    <link href="${esc(url)}"/>
         |    <updated>${r.releasedAt}</updated>
         |    <summary>${esc(summary)}</summary>
         |  </entry>""".stripMargin
    }
    s"""<?xml version="1.0" encoding="utf-8"?>
       |<feed xmlns="http://www.w3.org/2005/Atom">
       |  <id>$baseUrl/feed.xml</id>
       |  <title>New Scala releases · Scala Packages</title>
       |  <link href="$baseUrl/feed.xml" rel="self"/>
       |  <link href="$baseUrl/"/>
       |  <updated>$updated</updated>
       |  <author><name>Scala Packages</name></author>
       |${entries.mkString("\n")}
       |</feed>
       |""".stripMargin
