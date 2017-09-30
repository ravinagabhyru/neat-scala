/* Copyright 2017, Ravi Nagabhyru. All Rights Reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of
 * the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
 * WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
 * License for the specific language governing permissions and limitations under
 * the License.
 */

package sneat

import java.io.{BufferedWriter, FileWriter, PrintWriter}

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

object Util {
  def randfloat: Double = Random.nextDouble

  def randposneg: Int = {
    val n = Random.nextInt
    if ((n % 2) == 0) -1 else 1
  }

  def gaussrand: Double = Random.nextGaussian

  def randint(x: Int, y: Int): Int = x + Random.nextInt(y - x + 1)

  def randint(x: Int): Int = Random.nextInt(x - 1) + 1

  def writeToFile(fileName: String)(writerFunc: (PrintWriter) => Unit) : Unit = {
    val fileWriter = new FileWriter(fileName)
    val bufferedWriter = new BufferedWriter(fileWriter)
    val printWriter = new PrintWriter(bufferedWriter)

    writerFunc(printWriter)

    printWriter.flush()
    bufferedWriter.flush()
    fileWriter.flush()

    fileWriter.close()
  }

  def getRandomEntry[T](arrBuffer: ArrayBuffer[T]) = {
    val randEntry = randint(0, arrBuffer.size -1)
    arrBuffer(randEntry)
  }
}
