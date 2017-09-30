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

import java.io.PrintWriter

import scala.collection.mutable.ArrayBuffer

/**
  * A Trait is a group of parameters that can be expressed
  * as a group more than one time.  Traits save a genetic
  * algorithm from having to search vast parameter landscapes
  * on every node.  Instead, each node can simply point to a trait
  * and those traits can evolve on their own
  */
class Trait(val trait_id: Int, val params: ArrayBuffer[Double] = ArrayBuffer[Double]()) {

  def mutate(): Unit = {
    (0 until Neat.p_num_trait_params) foreach { i =>
      if (Util.randfloat > Neat.p_trait_param_mut_prob) {
        params(i) += (Util.randposneg * Util.randfloat) * Neat.p_trait_mutation_power
        if (params(i) < 0) params(i) = 0
      }
    }
  }

  def viewtext(header: String): Unit = {
    print(s"$header id = $trait_id , params = [")
    params foreach (" " + print(_))
    println(" ]")
  }

  def op_view(): Unit = {
    print(s" Trait # $trait_id \t")
    params foreach (" " + print(_))
    println()
  }
}

object Trait {
  def apply(t: Trait) = new Trait(t.trait_id, t.params)

  /**
    * creates a new trait from a string (line)
    * @param xline
    * @return Trait
    */
  def apply(xline: String) = {
    val st = xline.split("\\s+")

    // skip first word
    // Get the trait_id
    val trait_id = st(1).toInt

    // get real values.... starting from index 2
    val params : ArrayBuffer[Double] = ArrayBuffer[Double]()
    params ++= (0 until Neat.p_num_trait_params) map { index =>
      st(index + 2).toDouble
    }

    new Trait(trait_id, params)
  }

  /**
    * creates a new Trait which is the average of 2 existing traits passed in
    */
  def apply(t1: Trait, t2: Trait) = {
    val params : ArrayBuffer[Double] = ArrayBuffer[Double]()
    params ++= (0 until Neat.p_num_trait_params) map { i =>
      (t1.params(i) + t2.params(i)) / 2.0
    }

    new Trait(t1.trait_id, params)
  }

  def apply(id: Int, p1: Double, p2: Double, p3: Double, p4: Double, p5: Double, p6: Double, p7: Double, p8: Double, p9: Double) = {

    val params : ArrayBuffer[Double] = ArrayBuffer[Double]()
    params.append(p1)
    params.append(p2)
    params.append(p3)
    params.append(p4)
    params.append(p5)
    params.append(p6)
    params.append(p7)
    params.append(0)

    new Trait(id, params)
  }

  def print_to_file(t: Trait, printWriter: PrintWriter): Unit = {
    val str = t.params.foldLeft(s"trait ${t.trait_id} ")( (s, v) => s + " " + v )
    printWriter.println(str)
  }
}