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

import ai.onnxruntime.OnnxTensor
import ai.onnxruntime.OrtEnvironment
import ai.onnxruntime.OrtSession
import cats.effect.IO
import cats.effect.Ref
import cats.effect.Resource
import cats.effect.std.Semaphore
import cats.syntax.all.*

import java.nio.file.Files
import java.nio.file.Path
import scala.jdk.CollectionConverters.*
import scala.util.Using

/** Sentence embeddings from a local bge-small-en-v1.5 ONNX model: the [CLS]
  * hidden state, L2-normalised, so cosine similarity is a dot product. Queries
  * get bge's retrieval instruction prefix; documents do not.
  */
final class Embedder private (
    env: OrtEnvironment,
    session: OrtSession,
    tokenizer: WordPiece,
    batches: Semaphore[IO]
):
  import Embedder.*

  private val inputNames = session.getInputNames.asScala.toSet

  /** One search query, cut to 64 tokens, with the query instruction prefix. */
  def embedQuery(text: String): IO[Array[Float]] =
    IO.blocking(run(List(queryInstruction + text), maxTokens = 64).head)

  /** Search documents for `EmbedJob`, each cut to 256 tokens and run 16 at a
    * time, in input order.
    */
  def embedDocuments(texts: List[String]): IO[List[Array[Float]]] =
    batches.permit.surround(
      texts
        .grouped(16)
        .toList
        .flatTraverse(batch => IO.blocking(run(batch, maxTokens = 256)))
    )

  /** Search text -> pgvector literal, remembering recent queries (all forgotten
    * when 5000 are held).
    */
  def queryVectors: IO[String => IO[String]] =
    Ref.of[IO, Map[String, String]](Map.empty).map { cache => q =>
      cache.get.map(_.get(q)).flatMap {
        case Some(v) => IO.pure(v)
        case None    =>
          embedQuery(q)
            .map(toVectorLiteral)
            .flatTap(v =>
              cache.update(m =>
                (if m.size >= 5000 then Map.empty else m) + (q -> v)
              )
            )
      }
    }

  private def run(texts: List[String], maxTokens: Int): List[Array[Float]] =
    val encoded = texts.map(tokenizer.encode(_, maxTokens))
    val len = encoded.map(_.size).max
    val ids = encoded
      .map(e =>
        (e ++ Vector.fill(len - e.size)(tokenizer.pad)).map(_.toLong).toArray
      )
      .toArray
    val mask = encoded
      .map(e => Array.tabulate(len)(i => if i < e.size then 1L else 0L))
      .toArray
    val types = Array.fill(texts.size)(Array.fill(len)(0L))
    Using.Manager { use =>
      val inputs = Map(
        "input_ids" -> use(OnnxTensor.createTensor(env, ids)),
        "attention_mask" -> use(OnnxTensor.createTensor(env, mask)),
        "token_type_ids" -> use(OnnxTensor.createTensor(env, types))
      ).filter((name, _) => inputNames.contains(name))
      val result = use(session.run(inputs.asJava))
      // last_hidden_state: [batch][seq][hidden]; bge pools with [CLS]
      val hidden =
        result.get(0).getValue.asInstanceOf[Array[Array[Array[Float]]]]
      hidden.toList.map(seq => normalize(seq(0)))
    }.get

  private def normalize(v: Array[Float]): Array[Float] =
    val norm = math.sqrt(v.map(x => x.toDouble * x).sum).toFloat
    if norm == 0f then v else v.map(_ / norm)

object Embedder:

  val dimensions = 384

  private val queryInstruction =
    "Represent this sentence for searching relevant passages: "

  /** Opens `model.onnx` + `vocab.txt` from `dir`. Background batches take a
    * permit so they never use more than one model run at a time; queries run
    * alongside them.
    */
  def load(dir: Path): Resource[IO, Embedder] =
    val modelFile = dir.resolve("model.onnx")
    val vocabFile = dir.resolve("vocab.txt")
    val env = OrtEnvironment.getEnvironment()
    val session = Resource.fromAutoCloseable(IO.blocking {
      require(Files.exists(modelFile), s"missing $modelFile (run fetchModel)")
      val opts = new OrtSession.SessionOptions()
      opts.setIntraOpNumThreads(2)
      opts.setOptimizationLevel(OrtSession.SessionOptions.OptLevel.ALL_OPT)
      env.createSession(modelFile.toString, opts)
    })
    for
      s <- session
      tokenizer <- Resource.eval(IO.blocking(WordPiece.load(vocabFile)))
      batches <- Resource.eval(Semaphore[IO](1))
    yield new Embedder(env, s, tokenizer, batches)

  /** pgvector text literal, e.g. `[0.1,0.2]`. */
  def toVectorLiteral(v: Array[Float]): String = v.mkString("[", ",", "]")
