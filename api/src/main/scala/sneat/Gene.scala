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
  * Is a genetic codification of gene;
  *
  * @param innovation_num is historical marking of node
  * @param mutation_num how much mutation has changed the link
  * @param enable is a flag: is TRUE the gene is enabled FALSE otherwise.
  * @param lnk is a reference to object for identify input/output node and features
  */
class Gene(val innovation_num: Double, var mutation_num: Double, var enable: Boolean, val lnk: Link) {
  def geneEquals(that: Gene): Boolean = {
    this.lnk.in_node.node_id == that.lnk.in_node.node_id &&
      this.lnk.out_node.node_id == that.lnk.out_node.node_id &&
      this.lnk.is_recurrent == that.lnk.is_recurrent
  }

  def op_view(): Unit = {
    val mask03 = " 0.000;-0.000"
    val fmt03 = new DecimalFormat(mask03)
    val mask5 = " 0000"
    val fmt5 = new DecimalFormat(mask5)

    print("\n [Link (" + fmt5.format(lnk.in_node.node_id))
    print("," + fmt5.format(lnk.out_node.node_id))
    print("]  innov (" + fmt5.format(innovation_num))
    print(", mut=" + fmt03.format(mutation_num) + ")")
    print(" Weight " + fmt03.format(lnk.weight))

    if (lnk.linktrait != null) print(" Link's trait_id " + lnk.linktrait.trait_id)
    if (enable == false) print(" -DISABLED-")
    if (lnk.is_recurrent) print(" -RECUR-")
  }
}

object Gene {
  def apply(g: Gene, tp: Trait, inode: NNode, onode: NNode) = {
    val lnk = new Link(g.lnk.weight, inode, onode, g.lnk.is_recurrent, tp)
    new Gene(g.innovation_num, g.mutation_num, g.enable, lnk)
  }

  def apply(tp: Trait, w: Double, inode: NNode, onode: NNode, recur: Boolean, innov: Double, mnum: Double, enable: Boolean = true) = {
    val lnk = new Link(w, inode, onode, recur, tp)
    new Gene(innov, mnum, enable, lnk)
  }

  def apply(xline: String, traits: ArrayBuffer[Trait], nodes: ArrayBuffer[NNode]) : Gene = {

    val arr = xline.split("\\s+")

    // skip keyword 'gene', which is arr(0)

    // Get trait_id
    val trait_num = arr(1).toInt

    // Get input node
    val inode_num = arr(2).toInt

    // Get output node
    val onode_num = arr(3).toInt

    // Get weight
    val weight = arr(4).toDouble

    // Get recur
    val recur = if (arr(5).toInt == 1) true else false

    // Get innovation num
    val innovation_num = arr(6).toDouble

    // Get mutation num
    val mutation_num = arr(7).toDouble

    // Get enable
    val enable = if (arr(8).toInt == 1) true else false

    val trait_ptr = traits.find(_.trait_id == trait_num).orNull
    val in_node = nodes.find(_.node_id == inode_num).orNull
    val out_node = nodes.find(_.node_id == onode_num).orNull

    val lnk = new Link(weight, in_node, out_node, recur, trait_ptr)
    new Gene(innovation_num, mutation_num, enable, lnk)
  }

  def create_avg_gene(_p1gene: Gene, _p2gene: Gene): Gene = {
    val lnktrait = if (Util.randfloat > 0.5)
      _p1gene.lnk.linktrait
    else
      _p2gene.lnk.linktrait

    // WEIGHTS AVERAGED HERE
    val weight = (_p1gene.lnk.weight + _p2gene.lnk.weight) / 2.0
    val in_node = if (Util.randfloat > 0.5) _p1gene.lnk.in_node else _p2gene.lnk.in_node
    val out_node = if (Util.randfloat > 0.5) _p1gene.lnk.out_node else _p2gene.lnk.out_node
    val ir_recur = if (Util.randfloat > 0.5) _p1gene.lnk.is_recurrent else _p2gene.lnk.is_recurrent

    val innov_num = _p1gene.innovation_num
    val mutation_num = (_p1gene.mutation_num + _p2gene.mutation_num) / 2.0

    val lnk = new Link(weight, in_node, out_node, ir_recur, lnktrait)
    new Gene(innov_num, mutation_num, true, lnk)
  }

  def print_to_file(g: Gene, printWriter: PrintWriter): Unit = {
    val traitId = if (g.lnk.linktrait != null) g.lnk.linktrait.trait_id else 0
    val recurrent = if (g.lnk.is_recurrent) 1 else 0
    val enableInt = if (g.enable) 1 else 0
    val str =
      s"gene $traitId ${g.lnk.in_node.node_id} ${g.lnk.out_node.node_id} ${g.lnk.weight} " +
        s"${recurrent} ${g.innovation_num} ${g.mutation_num} $enableInt"
    printWriter.println(str)
  }
}
