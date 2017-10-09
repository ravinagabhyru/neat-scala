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
  * A Population is a group of Organisms including their species
  */
class Population() {
  /**
    * The organisms in the Population
    */
  val organisms = ArrayBuffer[Organism]()
  /**
    * Species in the Population the species should comprise all the genomes
    */
  val species = ArrayBuffer[Species]()
  /**
    * For holding the genetic innovations of the newest generation
    */
  val innovations = ArrayBuffer[Innovation]()
  /**
    * The last generation played
    */
  var final_gen = 0
  /**
    * the mean of fitness in current epoch
    */
  var mean_fitness = .0
  /**
    * current variance in this epoch
    */
  var variance = .0
  /**
    * Is a current standard deviation in current epoch
    */
  var standard_deviation = .0
  /**
    * An integer that when above zero tells when the first winner appeared; the number is epoch number.
    */
  var winnergen = 0
  /**
    * maximum fitness. (is used for delta code and stagnation detection)
    */
  var highest_fitness = .0
  /**
    * If  too high, leads to delta coding process.
    */
  var highest_last_changed = 0
  /**
    * Current label number available for nodes
    */
  private var cur_node_id = 0
  /**
    * Current  number of innovation
    */
  private var cur_innov_num = .0

  var CURR_ORGANISM_CHAMPION : Organism = null
  var MIN_ERROR = 0.0

  var REPORT_SPECIES_CODA = ""
  var REPORT_SPECIES_CORPO = ""
  var REPORT_SPECIES_TESTA = ""

  def this(g: Genome, size: Int) {
    this()
    winnergen = 0
    highest_fitness = 0.0
    highest_last_changed = 0
    spawn(g, size)
  }

  /**
    * Special constructor to create a population of random
    * topologies uses
    * Genome (int i, int o, int n,int nmax, bool r, double linkprob)
    * See the Genome constructor for the argument specifications
    * the parameter are :
    * size = number of organisms
    * i    = number of inputs
    * o    = number of output
    * nmax = max index of nodes
    * r    = the net can be recurrent ?
    * linkprob = probability of connecting two nodes.
    */
  def this(size: Int, i: Int, o: Int, nmax: Int, r: Boolean, linkprob: Double) {
    this()

    // print("\n  -Making a random population of "+size+" organisms ");
    winnergen = 0
    highest_fitness = 0.0
    highest_last_changed = 0

//    var fname_prefix: String = Population.getJneatFileData(Population.PREFIX_GENOME_RANDOM)

    organisms ++= (0 until size) map {count =>
      val new_genome = new Genome(count, i, o, Util.randint(0, nmax), nmax, r, linkprob)
      new Organism(0, new_genome, 1)
    }

    cur_node_id = i + o + nmax + 1

    cur_innov_num = (i + o + nmax) * (i + o + nmax) + 1
    //		 print("\n  The first  node_id  available is "+cur_node_id);
    //		 print("\n  The first innov_num available is "+cur_innov_num);
    speciate()

    // backup of population
//    fname_prefix = Population.getJneatFileData(Population.NAME_CURR_POPULATION)
//    Population.print_to_filename(this, fname_prefix)
  }

  /**
    * Construct off of a file of Genomes
    */
  def this(xFileName: String) {
    this()

    winnergen = 0
    highest_fitness = 0.0
    highest_last_changed = 0
    cur_node_id = 0
    cur_innov_num = 0.0

    val xFile = io.Source.fromFile(xFileName).getLines()
    while (xFile.hasNext) {
      val xline = xFile.next()
      val strArray = xline.split("\\w+")

      // skip till genomestart is the first word in a line
      if (strArray(0).equalsIgnoreCase("genomestart")) {
        val idcheck = strArray(1).toInt

        val new_genome = new Genome(idcheck, xFile)
        organisms.append(new Organism(0, new_genome, 1))

        if (cur_node_id < new_genome.get_last_node_id)
          cur_node_id = new_genome.get_last_node_id
        if (cur_innov_num < new_genome.get_last_gene_innovnum)
          cur_innov_num = new_genome.get_last_gene_innovnum
      }
    }
    speciate()
  }

  def getCur_node_id: Int = cur_node_id

  def setCur_node_id(cur_node_id: Int): Unit = this.cur_node_id = cur_node_id

  def getCur_innov_num: Double = cur_innov_num

  def setCur_innov_num(cur_innov_num: Double): Unit = this.cur_innov_num = cur_innov_num

  def spawn(g: Genome, size: Int): Unit = {
    var newgenome: Genome = null

    organisms ++= (1 to size) map { count =>
      //	  print("\n Creating organism -> " + count);
      newgenome = g.duplicate(count)
      newgenome.mutate_link_weight(1.0, 1.0, NeatConstant.GAUSSIAN)
      new Organism(0.0, newgenome, 1)
    }

    // Keep a record of the innovation and node number we are on
    cur_node_id = newgenome.get_last_node_id
    cur_innov_num = newgenome.get_last_gene_innovnum

    // Separate the new Population into species
    speciate()
  }

  def viewtext(): Unit = {
    print("\n\n\n\t\t *P O P U L A T I O N*")
    print("\n\n\t This population has " + organisms.size + " organisms, ")
    print(species.size + " species :\n")

    organisms foreach (_.viewtext())
    species foreach (_.viewtext())
  }

  /**
    * epoch turns over a Population to
    * the next generation based on fitness
    */
  def epoch(generation: Int): Unit = {
    // precision checking
    var half_pop = 0
    var tmpi = 0
    var stolen_babies = 0

    // Number of babies to steal
    // al momento NUM_STOLEN=1
    val NUM_STOLEN = Neat.p_babies_stolen

    // Use Species' ages to modify the objective fitness of organisms
    // in other words, make it more fair for younger species
    // so they have a chance to take hold
    // Also penalize stagnant species
    // Then adjust the fitness using the species size to "share" fitness
    // within a species.
    // Then, within each Species, mark for death
    // those below survival_thresh * average
    species foreach (_.adjust_fitness())

    // Go through the organisms and add up their fitnesses to compute the
    // overall average
    val total = organisms.foldLeft(0.0)(_ + _.fitness)

    val total_organisms = organisms.size
    val overall_average = total / total_organisms

    // Now compute expected number of offspring for each individual organism
    organisms foreach {_organism =>
      _organism.expected_offspring = _organism.fitness / overall_average
    }

    // Now add those offspring up within each Species to get the number of
    // offspring per Species
    var skim = 0.0
    var total_expected = 0
    species foreach {_specie =>
      skim = _specie.count_offspring(skim)
      total_expected += _specie.expected_offspring
    }

    // Need to make up for lost foating point precision in offspring assignment
    // If we lost precision, give an extra baby to the best Species
    if (total_expected < total_organisms) {
      // find the Species expecting the most offsprings
      val best_specie = species.foldLeft(species.head) { case (_best, _other) =>
        if (_best.expected_offspring > _other.expected_offspring) _best else _other
      }

      // Give an extra offspring to the best species
      best_specie.expected_offspring += 1

      // count the total number expected offspring in all species
      // TODO: does this have to be counted again, isn't it total_expected + 1?
      val final_expected = species.foldLeft(0)(_ + _.expected_offspring)

      // If we still arent at total, there is a problem
      // Note that this can happen if a stagnant Species
      // dominates the population and then gets killed off by its age
      // Then the whole population plummets in fitness
      // If the average fitness is allowed to hit 0, then we no longer have
      // an average we can use to assign offspring.
      if (final_expected < total_organisms) {
        print("\n Sorry : Population .has DIED +")
        print("\n ------------------------------")
        species foreach { _specie => _specie.expected_offspring = 0 }
        best_specie.expected_offspring = total_organisms
      }
    }

    // Sort the population and mark for death those after survival_thresh * pop_size
    val sorted_species = species.sortWith { (_sx, _sy) =>
      val _ox = _sx.organisms.head
      val _oy = _sy.organisms.head
      _ox.orig_fitness >= _oy.orig_fitness
    }

    // sorted species has all species ordered : the species with orig_fitness maximum is first
    var curspecies = sorted_species.head
    val best_species_num = curspecies.id
    var rep1 = new StringBuffer("")

    //   	print("\n  The BEST specie is #" + best_species_num);
    rep1.append("\n  the BEST  specie is #" + best_species_num)

    // report current situation
    sorted_species foreach { _specie =>
      //	  	print("\n  orig fitness of Species #" + _specie.id);
      rep1.append("\n  orig fitness of Species #" + _specie.id)
      //	  	print(" (Size " + _specie.getOrganisms().size() + "): ");
      rep1.append(" (Size " + _specie.organisms.size + "): ")
      // 	  	print(" is " + ((Organism) (_specie.organisms.firstElement())).orig_fitness);
      rep1.append(" is " + (_specie.organisms.head).orig_fitness)
      // 	  	print(" last improved ");
      rep1.append(" last improved ")
      //	  	print(_specie.age - _specie.age_of_last_improvement);
      rep1.append(_specie.age - _specie.age_of_last_improvement)
      //	  	print(" offspring "+_specie.expected_offspring);
      rep1.append(" offspring " + _specie.expected_offspring)
    }

    REPORT_SPECIES_TESTA = rep1.toString
    rep1 = new StringBuffer("")
    curspecies = sorted_species.head

    //Check for Population-level stagnation
    curspecies.organisms.head.pop_champ = true

    if (curspecies.organisms.head.orig_fitness > highest_fitness) {
      highest_fitness = curspecies.organisms.head.orig_fitness
      highest_last_changed = 0
      //	  	print("\n    Good! Population has reached a new *RECORD FITNESS* -> " + highest_fitness);
      rep1.append("\n    population has reached a new *RECORD FITNESS* -> " + highest_fitness)
      // 01.06.2002
      CURR_ORGANISM_CHAMPION = curspecies.organisms.head
      MIN_ERROR = curspecies.organisms.head.error
    }
    else {
      highest_last_changed += 1
      REPORT_SPECIES_TESTA = ""
      //	  	print("\n  Are passed "+ highest_last_changed+ " generations from last population fitness record: "+ highest_fitness);
      rep1.append("\n    are passed " + highest_last_changed + " generations from last population fitness record: " + highest_fitness)
    }

    REPORT_SPECIES_CORPO = rep1.toString

    // Check for stagnation - if there is stagnation, perform delta-coding
    if (highest_last_changed >= Neat.p_dropoff_age + 5) {
      //------------------ block delta coding ----------------------------
      print("\n+  <PERFORMING DELTA CODING>")

      highest_last_changed = 0
      half_pop = Neat.p_pop_size / 2
      tmpi = Neat.p_pop_size - half_pop

      print("\n  Pop size is " + Neat.p_pop_size)
      print(", half_pop=" + half_pop + ",   pop_size - halfpop=" + tmpi)

      var _specie = sorted_species.head

      // the first organism of first species can have  offspring = 1/2 pop size
      _specie.organisms.head.super_champ_offspring = half_pop

      // the first species  can have offspring = 1/2 pop size
      _specie.expected_offspring = half_pop
      _specie.age_of_last_improvement = _specie.age

      if (!sorted_species.tail.isEmpty) {
        val sorted_species_tail = sorted_species.tail
        _specie = sorted_species_tail.head
        _specie.organisms.head.super_champ_offspring = half_pop

        // the second species  can have offspring = 1/2 pop size
        _specie.expected_offspring = half_pop
        _specie.age_of_last_improvement = _specie.age

        // at this moment the offpring is terminated : the remainder species has 0 offspring!
        for( _specie <- sorted_species_tail.tail) {
          _specie.expected_offspring = 0
        }
      }
      else {
        _specie.organisms.head.super_champ_offspring += Neat.p_pop_size - half_pop
        _specie.expected_offspring += Neat.p_pop_size - half_pop
      }
    }
    else {
      // --------------------------------- block baby stolen (if baby stolen > 0)  -------------------------
      //		print("\n   Starting with NUM_STOLEN = "+NUM_STOLEN);
      if (Neat.p_babies_stolen > 0) {
        // Take away a constant number of expected offspring from the worst few species
        stolen_babies = 0
        for (_specie <- sorted_species
             if stolen_babies < NUM_STOLEN
             if _specie.age > 5 && _specie.expected_offspring > 2) {

          //		print("\n ....STEALING!");
          tmpi = NUM_STOLEN - stolen_babies
          if ((_specie.expected_offspring - 1) >= tmpi) {
            _specie.expected_offspring -= tmpi
            stolen_babies = NUM_STOLEN
          }
          else {
            // Not enough here to complete the pool of stolen
            stolen_babies += _specie.expected_offspring - 1
            _specie.expected_offspring = 1
          }
        }

        //		 	print("\n stolen babies = "+ stolen_babies);
        // Mark the best champions of the top species to be the super champs
        // who will take on the extra offspring for cloning or mutant cloning
        // Determine the exact number that will be given to the top three
        // They get , in order, 1/5 1/5 and 1/10 of the stolen babies
        val tb_four = new Array[Int](3)
        tb_four(0) = Neat.p_babies_stolen / 5
        tb_four(1) = tb_four(0)
        tb_four(2) = Neat.p_babies_stolen / 10

        var done = false
        var i_block = 0
        for (_specie <- sorted_species) {
          if (_specie.last_improved <= Neat.p_dropoff_age) if (i_block < 3) {
            if (stolen_babies >= tb_four(i_block)) {
              _specie.organisms.head.super_champ_offspring = tb_four(i_block)
              _specie.expected_offspring += tb_four(i_block)
              stolen_babies -= tb_four(i_block)
              print("\n  give " + tb_four(i_block) + " babies to specie #" + _specie.id)
            }
            i_block += 1
          }
          else if (i_block >= 3) {
            if (Util.randfloat > 0.1) if (stolen_babies > 3) {
              _specie.organisms.head.super_champ_offspring = 3
              _specie.expected_offspring += 3
              stolen_babies -= 3
              print("\n    Give 3 babies to Species " + _specie.id)
            }
            else {
              _specie.organisms.head.super_champ_offspring = stolen_babies
              _specie.expected_offspring += stolen_babies
              print("\n    Give " + stolen_babies + " babies to Species " + _specie.id)
              stolen_babies = 0
            }
            if (stolen_babies == 0) done = true
          }
        }
        if (stolen_babies > 0) {
          print("\n Not all given back, giving to best Species")
          val _specie = sorted_species(0)
          _specie.organisms.head.super_champ_offspring += stolen_babies
          _specie.expected_offspring += stolen_babies

          print("\n    force +" + stolen_babies + " offspring to Species " + _specie.id)
          stolen_babies = 0
        }
        // end baby_stolen > 0
      }
    }

    // ---------- phase of elimination of organism with flag eliminate ------------
    val vdel = organisms filter (_.eliminate) map { _organism =>
      // Remove the organism from its Species
      _organism.species.remove_org(_organism)
      // store the organism can be elimanated;
      _organism
    }
    // eliminate organism from master list
    organisms --= vdel

    // ---------- phase of reproduction -----------
    /*   	 print("\n ---- Reproduction at time " + generation+" ----");
        print("\n    species   : "+ sorted_species.size());
        print("\n    organisms : "+ organisms.size());
        print("\n    cur innov num : "+cur_innov_num);
        print("\n    cur node num  : "+cur_node_id);
        print("\n ---------------------------------------------");
        print("\n Start reproduction of species ....");
        */

    // print("\n verifica");
    // print("\n this species has "+sorted_species.size()+" elements");
    sorted_species foreach (_.reproduce(generation, this, sorted_species))

    //   	print("\n Reproduction completed");
    // Destroy and remove the old generation from the organisms and species
    // (because we have pointer to organisms , the new organisms created
    //  are not in organisms and can't br eliminated;
    // thus are elimate onlyu corrent organisms !)
    organisms foreach {_organism => _organism.species.remove_org(_organism)}
    organisms.clear()

    // Remove all empty Species and age ones that survive
    // As this happens, create master organism list for the new generation

    // remove all species with empty organisms
    species --= species filter (_.organisms.isEmpty)

    // update the age of species or novel flag
    species foreach {_specie =>
      // Age any Species that is not newly created in this generation
      if (_specie.novel)
        _specie.novel = false
      else {
        _specie.age += 1
      }

      // from the current species, reconstruct the master list organisms
      for (_organism <- _specie.organisms) {
        _organism.genome.phenotype.net_id = _organism.genome.genome_id
        organisms.append(_organism)
      }
    }

    // print("\n the number of species can be eliminated is "+vdel.size());
    // eliminate species marked from master list

    // Remove the innovations of the current generation
    innovations.clear()

    // DEBUG: Check to see if the best species died somehow
    // We don't want this to happen
    val best_ok = species.map(_.id).contains(best_species_num)
    /*
        if (!best_ok)
        print("\n  <ALERT>  THE BEST SPECIES DIED!");
        else
        print("\n  Good : the best Specie #" + best_species_num+" survived ");
        */
    if (!best_ok)
      REPORT_SPECIES_CODA = "\n  <ALERT>  THE BEST SPECIES DIED!"
    else
      REPORT_SPECIES_CODA = "\n  Good : the best Specie #" + best_species_num + " survived "
    //   	print("\n Epoch complete");
  }

  def speciate(): Unit = {
    species.clear()

    organisms foreach {_organism =>
      val specie_opt =  species find {_specie =>
        val compare_org = _specie.organisms.head

        // compare _organism-esimo('_organism') with first organism in current specie('compare_org')
        val curr_compat = _organism.genome.compatibility(compare_org.genome)

        (curr_compat < Neat.p_compat_threshold)
      }

      val newspecies = specie_opt.getOrElse {
        val newspecies = new Species(Species.get_next_species_num)
        species.append(newspecies)
        newspecies
      }

      newspecies.add_Organism(_organism)

      // update in organism pointer to its species
      _organism.species = newspecies
    }
  }

  /**
    * the increment of cur_node_id must be
    * executed only from a method of population
    * for security reason
    */
  def getCur_node_id_and_increment = {
    cur_node_id += 1
    cur_node_id - 1
  }

  /**
    * the increment of cur_innov_num must be
    * executed only from a method of population
    * for security reason
    */
  def getCurr_innov_num_and_increment = {
    cur_innov_num += 1
    cur_innov_num - 1
  }

  /**
    * Debug Population
    * Note: This checks each genome by verifying each one
    * Only useful for debugging
    */
  def verify() = {
    organisms.foreach(_.genome.verify)
  }
}

object Population {

  def run(pop: Population, numGenerations: Int, reporter: Reporter = new NullReporter)(evaluate: Organism => Boolean) = {
    pop.verify()

    // reproduce until a winner is found or up to numGenertions
    var foundWinner = false
    for (generation <- 1 to numGenerations if foundWinner == false) {
      reporter.startGeneration(pop, generation)

      // Evaluate each organism in parallel
      foundWinner = pop.organisms.foldLeft(foundWinner)((found, _organism) => found || evaluate(_organism))

      // compute average and max fitness for each species in parallel
      pop.species foreach {_specie =>
        _specie.compute_average_fitness()
        _specie.compute_max_fitness()
      }

      reporter.afterEvaluate(pop: Population, generation)

      // wait an epoch and make a reproduction of the best species
      pop.epoch(generation)

      reporter.endGeneration(pop, generation)
    }
  }

  def print_to_file_by_species(pop: Population, xNameFile: String) = { // write to file genome in native format (for re-read)
    Util.writeToFile(xNameFile) { printWriter =>
      pop.species.foreach(s => Species.print_to_file(s, printWriter))
    }
  }

  def print_to_filename(pop: Population, xNameFile: String) = {
    Util.writeToFile(xNameFile) { printWriter =>
      print_to_file(pop, printWriter)
    }
  }

  def print_to_file(pop: Population, printWriter: PrintWriter) = {
    pop.organisms.foreach(organism => Genome.print_to_file(organism.genome, printWriter))
  }
}
