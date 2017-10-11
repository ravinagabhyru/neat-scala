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

class Species(val id: Int, var novel: Boolean = false) {
  /**
    * The age of the Species
    */
  var age = 1
  /**
    * The average fitness of the Species
    */
  var ave_fitness = .0
  /**
    * Max fitness of the Species
    */
  var max_fitness = .0
  /**
    * The max it ever had
    */
  var max_fitness_ever = .0
  /**
    * how many child expected
    */
  var expected_offspring = 0
  /**
    * has tested ?
    */
  var checked = false
  /**
    * list of all organisms in the Species
    */
  var organisms: ArrayBuffer[Organism] = ArrayBuffer[Organism]()
  /**
    * how many time from last updt?
    * If this is too long ago, the Species will goes extinct.
    */
  var age_of_last_improvement = 0

  /**
    * add an organism to list of organisms in this specie
    */
  def add_Organism(xorganism: Organism): Unit = organisms.append(xorganism)

  /**
    * Can change the fitness of the organisms
    * in the Species to be higher for very new species
    * (to protect them);
    * Divides the fitness by the size of the Species,
    * so that fitness is "shared" by the species
    * At end mark the organisms can be eliminated from this specie
    */
  def adjust_fitness(): Unit = {
    val age_debt = {
      val tmp_age_debt = (age - age_of_last_improvement + 1) - Neat.p_dropoff_age
      if (tmp_age_debt == 0) 1 else tmp_age_debt
    }

    for (_organism <- organisms) {
      // Remember the original fitness before it gets modified
      _organism.orig_fitness = _organism.fitness

      // Make fitness decrease after a stagnation point dropoff_age
      // Added an if to keep species pristine until the dropoff point
      if (age_debt >= 1) {
        _organism.fitness = _organism.fitness * 0.01
        //		 	print("\n dropped fitness to " + _organism.fitness);
      }

      // Give a fitness boost up to some young age (niching)
      // The age_significance parameter is a system parameter
      //  if it is 1, then young species get no fitness boost
      if (age <= 10) _organism.fitness = _organism.fitness * Neat.p_age_significance

      // Do not allow negative fitness
      if (_organism.fitness < 0.0) _organism.fitness = 0.0001

      // Share fitness with the species
      _organism.fitness = _organism.fitness / organisms.size
    }

    // Sort the population in desceding order of fitness
    // and mark for death those after survival_thresh * pop_size
    organisms = organisms.sortWith { (_ox, _oy) =>
      // sort descending
      _ox.fitness >= _oy.fitness
    }

    // Update age_of_last_improvement here
    // (the first organism has the best fitness)
    if (organisms.head.orig_fitness > max_fitness_ever) {
      age_of_last_improvement = age
      max_fitness_ever = organisms.head.orig_fitness
    }

    // Decide how many get to reproduce based on survival_thresh*pop_size
    // Adding 1.0 ensures that at least one will survive
    // floor is the largest (closest to positive infinity) double value that is not greater
    // than the argument and is equal to a mathematical integer
    val num_parents = Math.floor((Neat.p_survival_thresh * organisms.size.toDouble) + 1.0).toInt

    //Mark for death those who are ranked too low to be parents
    //Mark the champ as such
    organisms.head.champion = true

    val (_, toEliminate) = organisms.splitAt(num_parents)
    toEliminate.foreach(_.eliminate = true)
  }

  /**
    * Read all organisms in this species and compute
    * the summary of fitness;
    * at and  compute the average fitness (ave_fitness)
    * with :    ave_fitness = summary / (number of organisms)
    * this is an average fitness for this specie
    */
  def compute_average_fitness(): Unit = {
    ave_fitness = organisms.map(_.fitness).sum / organisms.size.toDouble
  }

  /**
    * Read all organisms in this specie and return
    * the maximum fitness of all organisms.
    */
  def compute_max_fitness(): Unit = {
    max_fitness = organisms.map(_.fitness).max
  }

  /**
    * Compute the collective offspring the entire
    * species (the sum of all organism's offspring)
    * is assigned
    * skim is fractional offspring left over from a
    * previous species that was counted.
    * These fractional parts are kept unil they add
    * up to 1
    */
  def count_offspring(skim: Double): Double = {
    expected_offspring = 0

    val y1 = 1.0
    var r1 = 0.0
    var r2 = skim
    var n1 = 0
    var n2 = 0

    for (_organism <- organisms) {
      val x1 = _organism.expected_offspring
      n1 = (x1 / y1).toInt
      r1 = x1 - ((x1 / y1).toInt * y1)
      n2 = n2 + n1
      r2 = r2 + r1
      if (r2 >= 1.0) {
        n2 = n2 + 1
        r2 = r2 - 1.0
      }
    }
    expected_offspring = n2
    r2
  }

  def viewtext(): Unit = {
    println("\n +SPECIES : ")
    print("  id < " + id + " >")
    print(" age=" + age)
    print(", ave_fitness=" + ave_fitness)
    print(", max_fitness=" + max_fitness)
    print(", max_fitness_ever =" + max_fitness_ever)
    print(", expected_offspring=" + expected_offspring)
    print(", age_of_last_improvement=" + age_of_last_improvement)
    print("\n  This Species has " + organisms.size + " organisms :")
    print("\n ---------------------------------------")

    organisms foreach (_.viewtext())
  }

  /**
    * Compute generations since last improvement
    */
  def last_improved: Int = age - age_of_last_improvement

  /**
    * Eliminate the organism passed in parameter list,
    * from a list of organisms of this specie
    */
  def remove_org(org: Organism): Unit = {
    organisms -= org
  }

  // Do the mutation depending on probabilities of various mutations
  def mutate_genome(new_genome: Genome, pop: Population, generation: Int) : Boolean = {
    // Do the mutation depending on probabilities of various mutations
    val mut_struct_baby = if (Util.randfloat < Neat.p_mutate_add_node_prob) {
      new_genome.mutate_add_node(pop)
      true
    }
    else if (Util.randfloat < Neat.p_mutate_add_link_prob) {
      new_genome.genesis(generation)
      new_genome.mutate_add_link(pop, Neat.p_newlink_tries)
      true
    }
    else {
      // If we didn't do a structural mutation, we do the other kinds
      if (Util.randfloat < Neat.p_mutate_random_trait_prob)
        new_genome.mutate_random_trait

      if (Util.randfloat < Neat.p_mutate_link_trait_prob)
        new_genome.mutate_link_trait(1)

      if (Util.randfloat < Neat.p_mutate_node_trait_prob)
        new_genome.mutate_node_trait(1)

      if (Util.randfloat < Neat.p_mutate_link_weights_prob) {
        val mut_power = Neat.p_weight_mut_power
        new_genome.mutate_link_weight(mut_power, 1.0, NeatConstant.GAUSSIAN)
      }

      if (Util.randfloat < Neat.p_mutate_toggle_enable_prob)
        new_genome.mutate_toggle_enable(1)

      if (Util.randfloat < Neat.p_mutate_gene_reenable_prob)
        new_genome.mutate_gene_reenable

      false
    }

    mut_struct_baby
  }

  def reproduce(generation: Int, pop: Population, sorted_species: ArrayBuffer[Species]): Seq[Organism] = {
    // When a Species is found
    var champ_done = false

    if ((expected_offspring > 0) && organisms.isEmpty) {
      print("\n ERROR:  ATTEMPT TO REPRODUCE OUT OF EMPTY SPECIES")
      return Seq()
    }

    // elements for this specie
    val poolsize = organisms.size - 1

    // the champion of the 'this' specie is the first element of the specie;
    val thechamp = organisms.head

    val newOrganisms = (0 until expected_offspring) map { count =>
      var baby: Organism = null
      var mut_struct_baby = false
      var mate_baby = false

      if (expected_offspring > Neat.p_pop_size)
        print("\n ALERT: EXPECTED OFFSPRING = " + expected_offspring)

      // If we have a super_champ (Population champion), finish off some special clones
      if (thechamp.super_champ_offspring > 0) {
        // save in mom current champ;
        val mom = thechamp

        // create a new genome from this copy
        val new_genome = mom.genome.duplicate(count)

        if (thechamp.super_champ_offspring > 1) {
          if ((Util.randfloat < .8) || (Neat.p_mutate_add_link_prob == 0.0)) {
            val mut_power = Neat.p_weight_mut_power
            new_genome.mutate_link_weight(mut_power, 1.0, NeatConstant.GAUSSIAN)
          }
          else {
            // Sometimes we add a link to a superchamp
            new_genome.genesis(generation)
            new_genome.mutate_add_link(pop, Neat.p_newlink_tries)
            mut_struct_baby = true
          }
        }

        baby = new Organism(0.0, new_genome, generation)

        if (thechamp.super_champ_offspring == 1 && thechamp.pop_champ) {
          // print("\n The new org baby's (champion) genome is : "+baby.genome.getGenome_id());
          baby.pop_champ_child = true
          baby.high_fit = mom.orig_fitness
        }

        thechamp.super_champ_offspring -= 1
      }
      else if ((!champ_done) && (expected_offspring > 5)) {
        // If we have a Species champion, just clone it
        val mom = thechamp // Mom is the champ
        val new_genome = mom.genome.duplicate(count)
        champ_done = true

        // create the baby
        baby = new Organism(0.0, new_genome, generation)
      }
      else {
        // Choose the random mom
        val mom = Util.getRandomEntry(organisms)

        if ((Util.randfloat < Neat.p_mutate_only_prob) || poolsize == 1) {
          val new_genome = mom.genome.duplicate(count)
          mut_struct_baby = mutate_genome(new_genome, pop, generation)

          // create the baby
          baby = new Organism(0.0, new_genome, generation)
        }
        else {
          // Choose random dad, mate within Species
          val _dad = if (Util.randfloat > Neat.p_interspecies_mate_rate) {
            Util.getRandomEntry(organisms)
          }
          else {
            // try to find a random outside species to mate
            // if not found in 5 tries, then mate within species
            val randspecies = (0 until 5) map { _ =>
              val randmult = Math.min(1.0, Util.gaussrand / 4)

              // This tends to select better species
              val randspeciesnum = Math.floor((randmult * (sorted_species.size - 1.0)) + 0.5).toInt

              // randspeciesnum can be -ve
              val tmpNum = Math.max(0, randspeciesnum)
              sorted_species(tmpNum)
            } find (_ ne this) getOrElse(this)

            randspecies.organisms.head
          }

          val new_genome = if (Util.randfloat < Neat.p_mate_multipoint_prob) {
            // print("\n    mate multipoint baby: ");
            mom.genome.mate_multipoint_random(_dad.genome, count, mom.orig_fitness, _dad.orig_fitness)
          }
          else if (Util.randfloat < (Neat.p_mate_multipoint_avg_prob / (Neat.p_mate_multipoint_avg_prob + Neat.p_mate_singlepoint_prob))) { // print("\n    mate multipoint_avg baby: ");
            mom.genome.mate_multipoint_avg(_dad.genome, count, mom.orig_fitness, _dad.orig_fitness)
          }
          else {
            // print("\n    mate siglepoint baby: ");
            mom.genome.mate_singlepoint(_dad.genome, count)
          }
          mate_baby = true

          // Determine whether to mutate the baby's Genome
          // This is done randomly or if the mom and dad are the same organism
          if ((Util.randfloat > Neat.p_mate_only_prob) ||
            (_dad.genome.genome_id == mom.genome.genome_id) ||
            (_dad.genome.compatibility(mom.genome) == 0.0)) {
            mut_struct_baby = mutate_genome(new_genome, pop, generation)
          }

          // create the baby
          baby = new Organism(0.0, new_genome, generation)
        }
      }

      baby.mut_struct_baby = mut_struct_baby
      baby.mate_baby = mate_baby

      baby
    }

    newOrganisms
  }
}

object Species {
  /**
    * The highest species number
    */
  private var last_species = 0

  def get_next_species_num = {
    last_species = last_species + 1
    last_species
  }

  /**
    * Print to file all statistics information for this specie;
    * are information for specie, organisms,winner if present and genome
    */
  def print_to_file(s: Species, printWriter: PrintWriter) = {
    val mask4 = " 000"
    val fmt4 = new DecimalFormat(mask4)
    val mask13 = " 0.000"
    val fmt13 = new DecimalFormat(mask13)

    //Print a comment on the Species info
    var s2 = new StringBuffer("/* Species #")
    s2.append(fmt4.format(s.id))
    s2.append("         : (size=")
    s2.append(fmt4.format(s.organisms.size))
    s2.append(") (AvfFit=")
    s2.append(fmt13.format(s.ave_fitness))
    s2.append(") (Age=")
    s2.append(fmt13.format(s.age))
    s2.append(")  */")

    printWriter.println(s2.toString)

    //   	print("\n" + s2);
    s2 = new StringBuffer("/*-------------------------------------------------------------------*/")
    printWriter.println(s2.toString)

    for (_organism <- s.organisms) {
      s2 = new StringBuffer("/* Organism #")
      s2.append(fmt4.format(_organism.genome.genome_id))
      s2.append(" Fitness: ")
      s2.append(fmt13.format(_organism.fitness))
      s2.append(" Error: ")
      s2.append(fmt13.format(_organism.error))
      s2.append("                      */")
      printWriter.println(s2.toString)

      if (_organism.winner) {
        s2 = new StringBuffer("/*  $  This organism is WINNER with genome_id ")
        s2.append(fmt4.format(_organism.genome.genome_id))
        s2.append(" Species #")
        s2.append(fmt4.format(s.id))
        s2.append(" $   */")
        printWriter.println(s2.toString)
      }
      Genome.print_to_file(_organism.genome, printWriter)
    }

    s2 = new StringBuffer("/*-------------------------------------------------------------------*/")
    printWriter.println(s2.toString)
  }

  /**
    * Called for printing in a file statistics information
    * for this specie.
    */
  def print_to_filename(s: Species, xNameFile: String): Unit = { //
    // write to file genome in native format (for re-read)
    Util.writeToFile(xNameFile) { printWriter =>
      print_to_file(s, printWriter)
    }
  }
}