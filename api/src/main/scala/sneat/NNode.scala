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
import java.text.DecimalFormat

import scala.collection.mutable.ArrayBuffer


/**
  * A NODE is either a NEURON or a SENSOR. If it's a sensor, it can be loaded with a value for output
  * If it's a neuron, it has a list of its incoming input signals Use an activation count to avoid flushing
  *
  * @param node_type type is either NEURON or SENSOR
  * @param node_id Numeric identification of node
  * @param gen_node_label Used for genetic marking of nodes. are 4  type of node : input,bias,hidden,output
  * @param nodetrait Points to a trait of parameters
  */
class NNode(val node_type: Int, val node_id: Int, val gen_node_label: Int = NeatConstant.HIDDEN,
            var nodetrait: Trait = null) extends Neat {
  var inner_level = 0
  var is_traversed = false
  /**
    * type is either SIGMOID ..or others that can be added
    */
  val activationFunc = Activations.fsigmoid _
  /**
    * The incoming activity before being processed
    */
  var activesum = .0
  /**
    * The total activation entering in this Node
    */
  var activation = .0
  /**
    * when are signal's  to node,  the node switch this field  from FALSE to  TRUE
    */
  var active_flag = false
  /**
    * not used
    */
  var output = .0
  /**
    * vector of real values for hebbian or other advanced function future
    */
  val params = new Array[Double](Neat.p_num_trait_params)
  /**
    * A list of pointers to incoming weighted signals from other nodes
    */
  val incoming = ArrayBuffer[Link]()
  /**
    * A list of pointers to links carrying this node's signal
    */
  val outgoing = ArrayBuffer[Link]()
  /**
    * this value is how many time this node are activated during activation of network
    */
  var activation_count = .0
  /**
    * activation value of node at time t-1; Holds the previous step's activation for recurrency
    */
  var last_activation = .0
  /**
    * activation value of node at time t-2 Holds the activation before  the previous step's
    */
  var last_activation2 = .0
  /**
    * Is a reference  to a  Node ; Has used for generate and point from a genetic node (genotype)  to a real node (fenotype)
    * during 'genesis' process
    *
    */
  var analogue: NNode = null
  /**
    * Is a  temporary reference  to a  Node ; Has used for generate a new genome during duplicate phase of genotype.
    *
    */
  var dup: NNode = null

  def derive_trait(t: Trait): Unit = {
    for (count <- 0 until Neat.p_num_trait_params)
      params(count) = if (t != null) t.params(count) else 0.0
  }

  def depth(xlevel: Int, mynet: Network, xmax_level_in: Int): Int = {
    var xmax_level = xmax_level_in

    // control for loop
    if (xlevel > 100) {
      print("\n ** DEPTH NOT DETERMINED FOR NETWORK WITH LOOP ")
      //	  	print("\n Fenotype is " + mynet.getNet_id());
      //	  	print("\n Genotype is  " + mynet.getNet_id());
      //	  	mynet.genotype.op_view();
      10
    }
    else {
      // Base Case
      if (this.node_type == NeatConstant.SENSOR) {
        xlevel
      }
      else {
        // recursion case
        for (_link <- incoming) {
          val _ynode = _link.in_node
          val cur_depth = if (!_ynode.is_traversed) {
            _ynode.is_traversed = true
            val tmp_depth = _ynode.depth(xlevel + 1, mynet, xmax_level)
            _ynode.inner_level = tmp_depth - xlevel - 1
            tmp_depth
          }
          else
            xlevel + 1 + _ynode.inner_level

          xmax_level = Math.max(cur_depth, xmax_level)
        }
        xmax_level
      }
    }
  }

  def get_active_out: Double = if (activation_count > 0) activation else 0.0

  /**
    * Return activation currently in node
    * from PREVIOUS (time-delayed) time step,
    * if there is one
    */
  def get_active_out_td: Double = if (activation_count > 1) last_activation else 0.0

  def op_view(): Unit = {
    val maskf = " #,##0"
    val fmtf = new DecimalFormat(maskf)
    val mask5 = " #,##0.000"
    val fmt5 = new DecimalFormat(mask5)
    if (node_type == NeatConstant.SENSOR) print("\n (Sensor)")
    if (node_type == NeatConstant.NEURON) print("\n (Neuron)")
    print(fmtf.format(node_id) + " activation count " + fmt5.format(activation_count) + " activation=" + fmt5.format(activation) + ")")
  }

  def sensor_load(value: Double): Boolean =
    if (node_type == NeatConstant.SENSOR) {
      // Time delay memory
      last_activation2 = last_activation
      last_activation = activation
      activation_count += 1 // Puts sensor into next time-step
      activation = value // ovviamente non viene applicata la f(x)!!!!
      true
    }
    else false

  def mark(xlevel: Int, mynet: Network): Boolean = {
    // loop control
    if (xlevel > 100) {
      //      print("\n ** DEPTH NOT DETERMINED FOR NETWORK WITH LOOP ");
      //      print("\n Network name is " + mynet.getNet_id());
      //      mynet.genotype.op_view();
      false
    }
    else {
      // base Case
      if (this.node_type == NeatConstant.SENSOR) {
        this.is_traversed = true
        true
      }
      else {
        // recurrency case
        var rc = true
        for (_link <- incoming; if rc == true) {
          if (!_link.in_node.is_traversed) {
            _link.in_node.is_traversed = true
            rc = _link.in_node.mark(xlevel + 1, mynet)
          }
        }
        rc
      }
    }
  }

  def resetNNode(): Unit = {
    activation_count = 0
    activation = 0
    last_activation = 0
    last_activation2 = 0

    // Flush back link
    incoming foreach (_link => _link.flush_link())
    outgoing foreach (_link => _link.flush_link())
  }
}

object NNode {
  //      Construct the node out of a file specification
  //      using given list of traits
  def apply(xline: String, traits: ArrayBuffer[Trait]) : NNode = {
    val arr = xline.split("\\s+")

    // Get the node parameters
    val node_id = arr(1).toInt
    val node_type = arr(3).toInt  //get type of node
    val gen_node_label = arr(4).toInt //get genetic type of node
    val _trait_id = arr(2).toInt  // get trait
    val nodetrait = traits.find(_trait => _trait.trait_id == _trait_id).orNull

    new NNode(node_type, node_id, gen_node_label, nodetrait)
  }

  def apply(n: NNode, t: Trait) : NNode = {
    new NNode(n.node_type, n.node_id, n.gen_node_label, t)
  }

  def print_to_file(n: NNode, printWriter: PrintWriter): Unit = {
    val traitId = if (n.nodetrait != null) n.nodetrait.trait_id else 0
    val str = s"node ${n.node_id} ${traitId} ${n.node_type} ${n.gen_node_label}"
    printWriter.println(str)
  }
}