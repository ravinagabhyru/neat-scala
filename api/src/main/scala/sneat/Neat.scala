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
  * Is a superclass for definition of all parameters , threshold and others values.
  */
object Neat {
  /**
    * Probability  of mutating a single trait param
    */
  var p_trait_param_mut_prob = .0
  /**
    * Power of mutation on a signle trait param
    */
  var p_trait_mutation_power = .0
  /**
    * Amount that mutation_num changes for a trait change inside a link
    */
  var p_linktrait_mut_sig = .0
  /**
    * Amount a mutation_num changes on a link connecting a node that changed its trait
    */
  var p_nodetrait_mut_sig = .0
  /**
    * The power of a linkweight mutation
    */
  var p_weight_mut_power = .0
  /**
    * Probability  that a link mutation which doesn't have to be recurrent will be made recurrent
    */
  var p_recur_prob = .0
  /**
    * factor multiply for gene not equal
    */
  var p_disjoint_coeff = .0
  /**
    * factor multiply for gene excedeed
    */
  var p_excess_coeff = .0
  /**
    * factor multiply weight difference
    */
  var p_mutdiff_coeff = .0
  /**
    * threshold under which two Genomes are the same species
    */
  var p_compat_threshold = 0.1
  /**
    * How much does age matter in epoch cycle
    */
  var p_age_significance = .0
  /**
    * Percent of ave fitness for survival
    */
  var p_survival_thresh = .0
  /**
    * Probability  of a non-mating reproduction
    */
  var p_mutate_only_prob = .0
  /**
    * Probability  of mutate trait
    */
  var p_mutate_random_trait_prob = .0
  /**
    * Probability  of mutate link trait
    */
  var p_mutate_link_trait_prob = .0
  /**
    * Probability  of mutate node trait
    */
  var p_mutate_node_trait_prob = .0
  /**
    * Probability  of mutate link weight
    */
  var p_mutate_link_weights_prob = .0
  /**
    * Probability  of mutate status ena->dis | dis-ena of gene
    */
  var p_mutate_toggle_enable_prob = .0
  /**
    * Probability  of switch status to ena of gene
    */
  var p_mutate_gene_reenable_prob = .0
  /**
    * Probability  of add a node to struct of genome
    */
  var p_mutate_add_node_prob = 0.2
  /**
    * Probability  of add a link to struct of genome
    */
  var p_mutate_add_link_prob = .0
  /**
    * Probability  of a mate being outside species
    */
  var p_interspecies_mate_rate = .0
  /**
    * Probability  of cross in a many point of two genome
    */
  var p_mate_multipoint_prob = .0
  /**
    * Probability  of cross in a many point of two genome with media
    */
  var p_mate_multipoint_avg_prob = .0
  /**
    * Probability  of cross in a single point of two genome
    */
  var p_mate_singlepoint_prob = .0
  /**
    * Probability  of mating without mutation
    */
  var p_mate_only_prob = .0
  /**
    * Probability of forcing selection of ONLY links that are naturally recurrent
    */
  var p_recur_only_prob = .0
  /**
    * Size of population
    */
  var p_pop_size = 500
  /**
    * Age where Species starts to be penalized
    */
  var p_dropoff_age = 0
  /**
    * Number of tries mutate_add_link will attempt to find an open link
    */
  var p_newlink_tries = 0
  /**
    * Tells to print population to file every n generations
    */
  var p_print_every = 0
  /**
    * The number of babies to siphen off to the champions
    */
  var p_babies_stolen = 0
  /**
    * The number of runs for an experiment
    */
  var p_num_runs = 1

  /**
    * number of a trait
    */
  var p_num_trait_params = 8

  val d_trait_param_mut_prob = "Prob. of mutating a single trait param"
  val d_trait_mutation_power = "Power of mutation on a single trait param"
  val d_linktrait_mut_sig = "Amount that mutation_num changes for a trait change inside a link"
  val d_nodetrait_mut_sig = "Amount a mutation_num changes on a link connecting a node that changed its trait"
  val d_recur_prob = "Prob. that a link mutation which doesn't have to be recurrent will be made recurrent"
  val d_weight_mut_power = "The power of a link weight mutation"
  val d_disjoint_coeff = "factor multiply for gene not equal"
  val d_excess_coeff = "factor multiply for gene excedeed"
  val d_mutdiff_coeff = "factor multply weight difference"
  val d_compat_threshold = "threshold under which two Genomes are the same species"
  val d_age_significance = "How much does age matter in epoch cycle"
  val d_survival_thresh = "Percent of average fitness for survival"
  val d_mutate_only_prob = "Probability of a non-mating reproduction"
  val d_mutate_random_trait_prob = "Probability of mutate trait"
  val d_mutate_link_trait_prob = "Probability of mutate link trait"
  val d_mutate_node_trait_prob = "Probability of mutate node trait"
  val d_mutate_link_weights_prob = "Probability of mutate link weight"
  val d_mutate_toggle_enable_prob = "Probability of mutate status ena->dis | dis-ena of gene"
  val d_mutate_gene_reenable_prob = "Probability of switch status to ena of gene"
  val d_mutate_add_node_prob = "Probability of add a node to struct of genome"
  val d_mutate_add_link_prob = "Probability of add a link to struct of genome"
  val d_interspecies_mate_rate = "Probability of a mate being outside species"
  val d_mate_multipoint_prob = "Probability of cross in a many point of two genome"
  val d_mate_multipoint_avg_prob = "Probability of cross in a many point of two genome with media"
  val d_mate_singlepoint_prob = "Probability of cross in a single point of two genome"
  val d_mate_only_prob = "Probability of mating without mutation"
  val d_recur_only_prob = "Probability of forcing selection of ONLY links that are naturally recurrent"
  val d_pop_size = "Size of population"
  val d_dropoff_age = "Age where Species starts to be penalized"
  val d_newlink_tries = "Number of tries mutate_add_link will attempt to find an open link"
  val d_print_every = "Tells to print population to file every n generations"
  val d_babies_stolen = "The number of babies to siphen off to the champions"
  val d_num_runs = "The number of runs for an experiment"
  val d_num_trait_params = "number of a trait"


  implicit def reflector(ref: AnyRef) = new {
    def getMethods : Array[(String, Any)] = ref.getClass.getMethods.map({ method => println(method.getName); (method.getName, method.invoke(ref))})
    def getV(name: String): Any = ref.getClass.getMethods.find(_.getName == name).get.invoke(ref)
    def setV(name: String, value: Any): Unit = ref.getClass.getMethods.find(_.getName == name + "_$eq").get.invoke(ref, value.asInstanceOf[AnyRef])
  }

  /**
    * ritorna il nome senza i primi 2 caratteri ('p_', 'd_');
    */
  def normalizeName(s: String): String = s.substring(2)

  def readParam(fileName: String): Boolean = {
    io.Source.fromFile(fileName).getLines().foreach { l =>
      val arr = l.split("\\s+")
      val tmp = Neat.getV(arr(0))
//      println(s"$l -> ${arr(0)} ${arr(1)} $tmp")
      tmp match {
        case i: Int => Neat.setV(arr(0), arr(1).toInt)
        case d: Double => Neat.setV(arr(0), arr(1).toDouble)
        case _ => Neat.setV(arr(0), arr(1))
      }
    }
    true
  }
}
