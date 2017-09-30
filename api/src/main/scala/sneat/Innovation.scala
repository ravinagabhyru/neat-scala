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
  * This class serves as a way to record innovations
  * specifically, so that an innovation in one genome can be
  * compared with other innovations in the same epoch, and if they
  * Are the same innovation, they can both be assigned the same innnovation number.
  * This class can encode innovations that represent a new link
  * forming, or a new node being added.  In each case, two
  * nodes fully specify the innovation and where it must have
  * occured.  (Between them)
  */
trait Innovation extends Neat {
  /**
    * Two nodes specify where the innovation took place : this is the node input
    */
  def node_in_id: Int = 0

  /**
    * Two nodes specify where the innovation took place : this is the node output
    */
  def node_out_id: Int = 0

  /**
    * The number assigned to the innovation
    */
  def innovation_num1: Double = 0.0

  /**
    * If this is a new node innovation,then there are 2 innovations (links)
    * added for the new node
    */
  def innovation_num2: Double = 0.0

  /**
    * If a link is added, this is its weight
    */
  def new_weight: Double = 0.0

  /**
    * If a link is added, this is its connected trait
    */
  def new_traitnum: Int = 0

  /**
    * If a new node was created, this is its node_id
    */
  def newnode_id: Int = 0

  /**
    * If a new node was created, this is
    * the innovnum of the gene's link it is being
    * stuck inside
    */
  def old_innov_num: Double = .0

  /**
    * is recurrent ?
    */
  def recur_flag: Boolean = false
}

class LinkInnovation(nin: Int, nout: Int, num1: Double, w: Double, t: Int) extends Innovation {
  override val node_in_id = nin
  override val node_out_id = nout
  override val innovation_num1 = num1
  override val new_weight = w
  override val new_traitnum = t
}

class NodeInnovation(nin: Int, nout: Int, num1: Double, num2: Double, newid: Int, oldinnov: Double) extends Innovation {
  override val node_in_id = nin
  override val node_out_id = nout
  override val innovation_num1 = num1
  override val innovation_num2 = num2
  override val newnode_id = newid
  override val old_innov_num = oldinnov
}