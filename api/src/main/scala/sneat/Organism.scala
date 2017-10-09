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
  * Organisms are Genomes and Networks with fitness information i.e. The genotype and phenotype together
  *
  * @param fitness A measure of fitness for the Organism
  * @param genome The Organism's genotype
  * @param generation Tells which generation this Organism is from
  */
class Organism(var fitness: Double, val genome: Genome, val generation: Int) {
  /**
    * Win marker (if needed for a particular task)
    */
  var winner = false
  /**
    * The Organism's phenotype
    */
  val net : Network = genome.genesis(genome.genome_id)
  /**
    * A fitness measure that won't change during adjustments
    */
  var orig_fitness = 0.0
  /**
    * Used just for reporting purposes
    */
  var error = .0
  /**
    * The Organism's Species
    */
  var species : Species = null
  /**
    * Number of children this Organism may have
    */
  var expected_offspring = .0
  /**
    * Marker for destruction of inferior Organisms
    */
  var eliminate = false
  /**
    * Marks the species champ
    */
  var champion = false
  /**
    * Number of reserved offspring for a population leader
    */
  var super_champ_offspring = 0
  /**
    * Marks the best in population
    */
  var pop_champ = false
  /**
    * Marks the duplicate child of a champion (for tracking purposes)
    */
  var pop_champ_child = false
  /**
    * DEBUG variable- high fitness of champ
    */
  var high_fit = .0
  /**
    * has a change in a structure of baby ?
    */
  var mut_struct_baby = false
  /**
    * has a mating  in  baby ?
    */
  var mate_baby = false

  def viewtext(): Unit = {
    print("\n-ORGANISM -[genomew_id=" + genome.genome_id + "]")
    print(" Champ(" + champion + ")")
    print(", fit=" + fitness)
    print(", Elim=" + eliminate)
    print(", offspring=" + expected_offspring)
  }
}