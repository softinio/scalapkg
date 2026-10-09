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

package com.softinio.scalapkg.embed

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.nio.file.Path
import java.text.Normalizer
import scala.collection.mutable.ArrayBuffer
import scala.jdk.CollectionConverters.*

/** The uncased BERT WordPiece tokenizer used by bge-small-en-v1.5: clean the
  * text, lowercase and strip accents, split on whitespace and punctuation, then
  * split each word into the longest vocabulary pieces (`##` marks a
  * continuation). Written out here so the app needs no native tokenizer.
  */
final class WordPiece(vocab: Map[String, Int]):

  private val unk = vocab("[UNK]")
  val cls: Int = vocab("[CLS]")
  val sep: Int = vocab("[SEP]")
  val pad: Int = vocab("[PAD]")

  private val maxWordChars = 100

  private def isPunctuation(c: Char): Boolean =
    (c >= 33 && c <= 47) || (c >= 58 && c <= 64) || (c >= 91 && c <= 96) ||
      (c >= 123 && c <= 126) || {
        Character.getType(c) match
          case Character.CONNECTOR_PUNCTUATION | Character.DASH_PUNCTUATION |
              Character.START_PUNCTUATION | Character.END_PUNCTUATION |
              Character.INITIAL_QUOTE_PUNCTUATION |
              Character.FINAL_QUOTE_PUNCTUATION | Character.OTHER_PUNCTUATION =>
            true
          case _ => false
      }

  private def isCjk(c: Char): Boolean =
    (c >= '一' && c <= '鿿') || (c >= '㐀' && c <= '䶿') ||
      (c >= '豈' && c <= '﫿')

  /** Lowercase, strip accents, drop control characters, and split into words
    * and single punctuation characters.
    */
  def basicTokens(text: String): Vector[String] =
    val lowered = Normalizer
      .normalize(text.toLowerCase, Normalizer.Form.NFD)
      .filterNot(c => Character.getType(c) == Character.NON_SPACING_MARK)
    val out = Vector.newBuilder[String]
    val word = new StringBuilder
    def flush(): Unit =
      if word.nonEmpty then
        out += word.toString
        word.clear()
    lowered.foreach { c =>
      if c == 0 || c == '�' || (Character.isISOControl(c) && !c.isWhitespace)
      then ()
      else if c.isWhitespace then flush()
      else if isPunctuation(c) || isCjk(c) then
        flush()
        out += c.toString
      else word += c
    }
    flush()
    out.result()

  /** Greedy longest-match-first split of one word into vocabulary ids. */
  def wordPieces(word: String): Vector[Int] =
    if word.length > maxWordChars then Vector(unk)
    else
      val pieces = ArrayBuffer.empty[Int]
      var start = 0
      var ok = true
      while ok && start < word.length do
        var end = word.length
        var found = -1
        while found < 0 && end > start do
          val sub = word.substring(start, end)
          val key = if start > 0 then s"##$sub" else sub
          vocab.get(key) match
            case Some(id) => found = id
            case None     => end -= 1
        if found < 0 then ok = false
        else
          pieces += found
          start = end
      if ok then pieces.toVector else Vector(unk)

  /** `[CLS] tokens [SEP]`, truncated to `maxLength` ids in total. */
  def encode(text: String, maxLength: Int): Vector[Int] =
    val ids = basicTokens(text).iterator.flatMap(wordPieces).take(maxLength - 2)
    (cls +: ids.toVector) :+ sep

object WordPiece:
  /** Reads a `vocab.txt` with one token per line; the line number is the id. */
  def load(vocabFile: Path): WordPiece =
    val lines = Files.readAllLines(vocabFile, StandardCharsets.UTF_8).asScala
    WordPiece(lines.iterator.zipWithIndex.map((t, i) => t -> i).toMap)
