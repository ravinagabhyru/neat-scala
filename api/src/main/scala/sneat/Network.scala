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

import scala.collection.mutable.ArrayBuffer

/**
  *
  * @param inputs   Is a collection of object NNode can be mapped in a Vector container
  *                 this collection represent a group of references to input nodes;
  * @param outputs  Is a collection of object NNode can be mapped in a Vector container;
  *                 this collection represent a group of references to output nodes;
  * @param allnodes Is a collection of object NNode can be mapped in a Vector container;
  *                 this collection represent a group of references to all nodes of this net;
  * @param net_id   Numeric identification of this network
  */
class Network(val inputs: ArrayBuffer[NNode], val outputs: ArrayBuffer[NNode],
              val allnodes: ArrayBuffer[NNode], var net_id: Int) {
  /**
    * is a reference to genotype can has  originate this fenotype
    */
  var genotype: Genome = null
  /**
    * Is a name of this network
    */
  var name: String = null
  /**
    * Number of NNodes of this net
    */
  var numnodes = -1
  /**
    * Number of Link  in this net
    */
  var numlinks = -1
  /**
    * are user for working scope ; at this moment
    * is utilized for returning code of a search if recurrency
    * if ==  8 , the net has a loop ;
    */
  var status = 0

  def activate: Boolean = {
    // try upto 30 times to activate nodes until all outputs are activated
    // if even after 30 tries, if all outputs are not activated, then outputs
    // are probably disconnected
    val outputs_activated = (0 until 30) exists { _ =>
      // For each node, compute the sum of its incoming activation
      allnodes filter (_.node_type != NeatConstant.SENSOR) foreach { _node =>
        _node.activesum = 0.0 // reset activation value
        _node.active_flag = false // flag node disabled

        for (_link <- _node.incoming) {
          if (!_link.time_delay) {
            val add_amount = _link.weight * _link.in_node.get_active_out
            if (_link.in_node.active_flag || _link.in_node.node_type == NeatConstant.SENSOR)
              _node.active_flag = true
            _node.activesum += add_amount
          }
          else {
            val add_amount = _link.weight * _link.in_node.get_active_out_td
            _node.activesum += add_amount
          }
        }
      }

      // Now activate all the non-sensor nodes off their incoming activation,
      // if some active input came in
      for (_node <- allnodes if _node.node_type != NeatConstant.SENSOR && _node.active_flag) {
        _node.last_activation2 = _node.last_activation
        _node.last_activation = _node.activation
        _node.activation = _node.activationFunc(_node.activesum, 4.924273, 2.4621365)
        _node.activation_count += 1.0
      }

      // check if there is an output that is not activated
      val outputs_not_activated = outputs exists (_.activation_count == 0)
      !outputs_not_activated
    }

    if (!outputs_activated) print("\n *ERROR* Inputs disconnected from output!")

    outputs_activated
  }

  // TODO: implement activation loop here
//  def activate(in: Array[Double]) = {
//    // The max depth of the network to be activated
//    val net_depth = max_depth
//
//    load_sensors(in)
//    activate_local
//
//    // next activation while last level is reached !
//    // use depth to ensure relaxation
//    for (_ <- 0 to net_depth) activate_local
//
//    flush()
//    outputs
//  }

  def flush() = {
    // new version : the number of connection >> num of node defined
    // thus is good to reset all nodes without respect connection
    allnodes foreach (_.resetNNode())
  }

  def load_sensors(sensvals: Array[Double]) = {
    val sensors = inputs filter (_.node_type == NeatConstant.SENSOR)
    for ((_node, value) <- sensors.zip(sensvals)) _node.sensor_load(value)
  }

  /**
    * Find the maximum number of neurons between
    * an ouput and an input
    */
  def max_depth = {
    var cur_depth = 0
    var max = 0
    for (_node <- allnodes) {
      _node.inner_level = 0
      _node.is_traversed = false
    }

    for (_node <- outputs) {
      cur_depth = _node.depth(0, this, max)
      if (cur_depth > max) max = cur_depth
    }
    max
  }

  def viewAllNodes(s: String) = {
    println(s)
    print("\n\t - List of all nodes -")

    allnodes.filter(_.active_flag).foreach(_node => _node.op_view())

    print("\n\t - end list of all nodes -")
  }

  def has_a_path(potin: NNode, potout: NNode, level: Int, threshold: Int) = {
    // reset all link to state no traversed
    allnodes foreach (_.is_traversed = false)

    // call the control if has a link intra node potin , potout
    is_recur(potin, potout, level, threshold)
  }

  /**
    * This module control if has at leat one
    * link from out and all sensor
    * It flow in all link and if at end are one sensor not
    * 'marked' , return false
    */
  def is_minimal: Boolean = {
    // reset all pending situation
    allnodes foreach (_.is_traversed = false)

    // attempted to excite all sensor
    for (_node <- outputs) {
      val rc = _node.mark(0, this)
      // the false conditions is for a net with loop
      // or an output without connection direct or indirect
      //
      if (rc == false) return false
    }

    // okay the virtual signal is flowed,
    // now control if all sensor of fenotype are touched :
    val ret_code = inputs exists (!_.is_traversed)
    !ret_code
  }

  def is_recur(potin_node: NNode, potout_node: NNode, levelIn: Int, thresh: Int): Boolean = {
    val level = levelIn + 1
    if (level > thresh) {
      status = 8
      return false
    }

    if (potin_node eq potout_node) true
    else {
      for (_link <- potin_node.incoming) {
        if (!_link.is_recurrent && !_link.in_node.is_traversed) {
          _link.in_node.is_traversed = true
          if (is_recur(_link.in_node, potout_node, level, thresh)) return true
        }
      }
      potin_node.is_traversed = true
      false
    }
  }

  /**
    * starting from sensor , send a signal forward the net
    * after all nodes are active , control if last
    * activation is == current activation in output node
    * if activation of output nodes remain stable for 'period'
    * interval , return the difference from total cycle and
    * time passed from fist level stable.
    */
  def is_stabilized(periodIn: Int): Int = {
    var add_amount = 0.0
    val period = if (periodIn == 0) 30 else periodIn

    // first step : activation of sensor nodes
    for (_node <- inputs if _node.node_type == NeatConstant.SENSOR) {
      _node.last_activation2 = _node.last_activation
      _node.last_activation = _node.activation
      _node.activation_count += 1
      _node.activation = 1
    }

    // activation of net
    var counter_stable = 0
    var level = 0
    val limit = period + 90
    val found = (0 until limit) exists { time_passed =>
      for (_node <- allnodes if _node.node_type != NeatConstant.SENSOR) {
        _node.activesum = 0.0
        _node.active_flag = false
        for (_link <- _node.incoming) {
          if (!_link.time_delay) {
            add_amount = _link.in_node.get_active_out
            if (_link.in_node.active_flag || _link.in_node.node_type == NeatConstant.SENSOR) _node.active_flag = true
            _node.activesum += add_amount
          }
          else {
            add_amount = _link.in_node.get_active_out_td
            _node.activesum += add_amount
          }
        }
      }

      for (_node <- allnodes if _node.node_type != NeatConstant.SENSOR && _node.active_flag) {
        _node.last_activation2 = _node.last_activation
        _node.last_activation = _node.activation
        _node.activation = _node.activesum
        _node.activation_count += 1.0
      }

      // check if there is an activattion on the outputs
      val outputsoff = outputs exists (_.activation_count == 0)

      // check if there is any change in activation, if there is, then it is not stable yet
      val has_changed = outputs exists (_node => _node.last_activation != _node.activation)

      // if no changes, increment counter, else reset and start again
      counter_stable = if (!has_changed) counter_stable + 1 else 0

      val done: Boolean = if (!outputsoff && !has_changed && counter_stable >= period) {
        level = time_passed
        true
      }
      else
        false

      done
    }

    // return delta = total time passed (real) - period (depth virtual), if done
    if (found) level - period + 1 else 0
  }
}