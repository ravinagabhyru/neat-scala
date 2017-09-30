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

/**
  * Link is a connection from one node to another with an associated weight; It can be marked as recurrent;
  * Its parameters are made public for efficiency.
  *
  * @param weight is a real value of weight of connection(link)
  * @param in_node is a reference to an  input node
  * @param out_node is a reference to a output node
  * @param is_recurrent this is a flag; if TRUE the connection(link) is recurrent; FALSE otherwise
  * @param linktrait Points to a trait of parameters for genetic creation.
  *                  Is  link-related parameters that change during Hebbian type learning.
  */
class Link(var weight: Double, val in_node: NNode, val out_node: NNode, val is_recurrent: Boolean, var linktrait: Trait = null)
  extends Neat {
  /**
    * this is a flag; if TRUE the connection(link) is tapped(delayed); FALSE otherwise;
    */
  var time_delay = false
  /**
    * The amount of weight adjustment
    */
  var added_weight = .0
  /**
    * Is  link-related parameters that change during Hebbian type learning.
    */
  var params = new Array[Double](Neat.p_num_trait_params)
  /**
    * this is a flag for compute depth; if TRUE the connection(link) is already analyzed; FALSE otherwise;
    */
  var is_traversed = false

  def derive_trait(t: Trait): Unit =
    for (count <- 0 until Neat.p_num_trait_params)
      params(count) = if (t != null) t.params(count) else 0.0

  def flush_link() : Unit = {
    added_weight = 0.0
    is_traversed = false
  }

  def viewtext(): Unit = {
    print("\n +LINK : ")
    print(s"weight=" + weight)
    print(", weight-add=" + added_weight)
    print(", i(" + in_node.node_id)
    print(")--<CONNECTION>--o(")
    print(out_node.node_id + ")")
    print(", recurrent=" + is_recurrent)
    print(", tapped=" + time_delay)

    if (linktrait != null)
      linktrait.viewtext("\n         (linktrait)-> ")
    else
      print("\n         *warning* linktrait for this gene is null ")
  }
}