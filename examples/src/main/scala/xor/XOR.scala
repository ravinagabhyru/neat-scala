package examples

import java.text.DecimalFormat

import sneat._

class XORReporter extends Reporter {
  val mask6 = "000000"
  val fmt6 = new DecimalFormat(mask6)
  var winner = false
  val fitnessThreshold = Math.pow(3.9, 2)

  override def startGeneration(pop: Population, generation: Int): Unit = {
    println("---------------- Generation ----------------------" + generation)
  }

  override def afterEvaluate(pop: Population, generation: Int): Unit = {
    var cnt = 0
    pop.organisms filter (_.fitness > fitnessThreshold) foreach { _organism =>
      println("   -WINNER IS #" + _organism.genome.genome_id)
      Genome.print_to_filename(_organism.genome, "data/xor_win" + cnt)
      cnt += 1
    }

    // Only print to file every print_every generations or if there are winners
    if (cnt > 0 || (generation % Neat.p_print_every) == 0) {
      val fileName = "g_" + fmt6.format(generation)
      Population.print_to_file_by_species(pop, "data/" + fileName)
    }
    if (cnt > 0) print("\t****** I HAVE FOUND A CHAMPION ******")
    winner = cnt > 0
  }

  override def endGeneration(pop: Population, generation: Int): Unit = {
    println("  Population : innov num   = " + pop.getCur_innov_num)
    println("             : cur_node_id = " + pop.getCur_node_id)
    println("   winner    : " + winner)
    println()
  }
}


object XOR {
  def xor_evaluate(organism: Organism): Boolean = {
    var _net: Network = null
    var success = false

    // The four possible input combinations to xor
    // The first number is for biasing
    val xorInputs = Array(Array(1.0, 0.0, 0.0), Array(1.0, 0.0, 1.0), Array(1.0, 1.0, 0.0), Array(1.0, 1.0, 1.0))
    val xorOutputs = Array(0.0, 1.0, 1.0, 0.0)

    _net = organism.net

    // The max depth of the network to be activated
    val net_depth = _net.max_depth

    var errorsum = 0.0
    for ( (in, out) <- xorInputs.zip(xorOutputs)) {
      _net.load_sensors(in)
      success = _net.activate

      // next activation while last level is reached !
      // use depth to en  sure relaxation
      for (_ <- 0 to net_depth) success = _net.activate

      // ok : the propagation is completed : repeat until all examples are presented
      val resOut = _net.outputs.head.activation
      _net.flush()

      errorsum += Math.abs(out - resOut)
    }

    if (success) {
      organism.fitness = Math.pow(4.0 - errorsum, 2)
      organism.error = errorsum
    }
    else {
      organism.fitness = 0.001
      organism.error = 999.0
    }

    organism.winner = organism.fitness > Math.pow(3.9, 2)
    organism.winner
  }

  /**
    * This is a sample of creating a new Population with
    * 'size_population' organisms , and simulation
    * of XOR example
    * This sample can be started in two modality :
    * -cold : each time the population is re-created from 0;
    * -warm : each time the population re-read last population
    * created and restart from last epoch.
    * (the population backup file is : 'data/population.primitive'
    */
  def XOR(size_population: Int, fromFile: Boolean, gens: Int): Unit = {
    var pop: Population = null
    val fname_prefix = "data/population.primitive"

    println("------ Start XOR Example -------")
    for (_ <- 0 until Neat.p_num_runs) {
      println(" Spawned population off genome")
      val prb_link = 0.50
      val recurrent = true

      // default cold is : 3 sensor (1 for bias) , 1 out , 5 nodes max, no recurrent
      if (!fromFile)
        pop = new Population(size_population, 3, 1, 5, recurrent, prb_link) // cold start-up
      else {
        pop = new Population(fname_prefix + ".last") // warm start-up
      }

      println()
      println("---------------- Generation starting with----------")
      println("  Population : innov num   = " + pop.getCur_innov_num)
      println("             : cur_node_id = " + pop.getCur_node_id)
      println("---------------------------------------------------")
      println()

      Population.run(pop, gens, new XORReporter)(xor_evaluate)
    }

    // backup of population for warm startup
    Population.print_to_filename(pop, fname_prefix + ".last")
    println("\n End of experiment")
  }

  def main(args: Array[String]): Unit = {
    val paramInFile = "data/parameters"
    if (Neat.readParam(paramInFile))
      println(" okay read ")
    else
      println(" error in read ")

    XOR(150, false, 50)
  }
}
