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
  *
  * @param genome_id Numeric identification for this genotype
  * @param traits Reserved parameter space for future use
  * @param nodes collection of NNode's
  * @param genes Each Gene in (3) has a marker telling when it arose historically;
  *              Thus, these Genes can be used to speciate the population, and
  *              the list of Genes provide an evolutionary history of innovation and link-building
  */
class Genome(val genome_id: Int,
             val traits: ArrayBuffer[Trait] = ArrayBuffer[Trait](),
             val nodes: ArrayBuffer[NNode] = ArrayBuffer[NNode](),
             val genes: ArrayBuffer[Gene] = ArrayBuffer[Gene]()) {
  /**
    * note are two String for store statistics information
    * when genomes are readed (if exist : null otherwise);
    */
  var notes: String = null
  /**
    * Is a reference from this genotype to fenotype
    */
  var phenotype: Network = null

  /**
    * Creation of a new random genome with :
    * new_id   = numerical identification of genome
    * i   = number of input nodes
    * o   = number of output nodes
    * n   = number of hidden nodes
    * nmax   = number max of node
    * this number must be >= (i + n + o)
    * r   = the network can have a nodes recurrent ?
    * linkprob = probability of a link from nodes ( must be in interval  ]0,1[);
    */
  def this(new_id: Int, nInputNodes: Int, nOutputNodes: Int, nHiddenNodes: Int, nMaxNodes: Int, r: Boolean, linkprobIn: Double) {
    this(new_id)

    //
    //    i i i n n n n n n n n n n n n n n n n . . . . . . . . o o o o
    //    |                                   |                 ^     |
    //    |<----------- maxnode ------------->|                 |     |
    //    |                                                     |     |
    //    |<-----------------------total nodes -----------------|---->|
    //                                                          |
    //     first output ----------------------------------------+

    // NOTE: Bias node is not added to totalNodes
    val totalNodes = nInputNodes + nOutputNodes + nMaxNodes

    val maxnode = nInputNodes + nHiddenNodes

    // Create a dummy trait (this is for future expansion of the system)

    val newtrait = Trait(1, 0, 0, 0, 0, 0, 0, 0, 0, 0)
    traits.append(newtrait)

    // build input nodes
    nodes ++= (1 until nInputNodes) map { node_id =>
      new NNode(NeatConstant.SENSOR, node_id, NeatConstant.INPUT, newtrait)
    }

    // create a bias node
    nodes += new NNode(NeatConstant.SENSOR, nInputNodes, NeatConstant.BIAS, newtrait)


    // build hidden nodes
    nodes ++= (0 until nHiddenNodes) map { node_id =>
      new NNode(NeatConstant.NEURON, nInputNodes + 1 + node_id, NeatConstant.HIDDEN, newtrait)
    }

    // Build the output nodes
    val first_output = totalNodes - nOutputNodes + 1
    nodes ++= (first_output to totalNodes) map { node_id =>
      new NNode(NeatConstant.NEURON, node_id, NeatConstant.OUTPUT, newtrait)
    }

    var forced_probability = 0.5
    var abort = 0

    var linkprob = linkprobIn
    var done = false
    while (!done) {
      assert(abort < 700, "SEVERE ERROR in genome random creation costructor : genome has not created")
      abort += 1

      if (abort >= 20) {
        //		    	print("\n ALERT  force new probability from 0.5 to 1 step .01");
        linkprob = forced_probability
        forced_probability += .01
      }

      //creation of connections matrix
      //Step through the connection matrix, randomly assigning bits
      val matrixdim = totalNodes * totalNodes
      val cmp = new Array[Boolean](matrixdim)
      for (count <- 0 until matrixdim) {
        cmp(count) = if (Util.randfloat < linkprob) true else false
      }

      //      val cmp = (0 until matrixdim) map (_ => NeatRoutine.randfloat < linkprob) toArray

      // Connect the nodes
      var innov_number = 0 //counter for labelling the innov_num  of genes

      // Step through the connection matrix, creating connection genes
      for (col <- 1 to totalNodes; row <- 1 to totalNodes) {
        if ((cmp(innov_number) && (col > nInputNodes)) &&
          (col <= maxnode || col >= first_output) &&
          (row <= maxnode || row >= first_output)) {

          // If it isn't recurrent, create the connection no matter what
          // create a gene if col > row or recurrent nodes are allowed
          if (col > row || r) {
            //Retrieve the in_node , out_node
            val in_node = nodes.find(_.node_id == row).orNull
            val out_node = nodes.find(_.node_id == col).orNull

            val flag_recurrent = (col <= row)

            //Create the gene + link
            val new_weight = Util.randposneg * Util.randfloat
            val newgene = Gene(newtrait, new_weight, in_node, out_node, flag_recurrent, innov_number, new_weight)

            //Add the gene to the genome
            genes.append(newgene)
          }
          // end condition for a correct link in genome
        }
        innov_number += 1
      }

      if (verify) {
        val net = genesis(genome_id)
        if (net.is_minimal) {
          val lx = net.max_depth
          val dx = net.is_stabilized(lx)
          //				print("\n        lx = " + lx);
          //				print(", dx = " + dx);
          if (((dx == lx) && (!r)) || ((lx > 0) && r && (dx == 0))) done = true
        }
        net.genotype = null
        this.phenotype = null
      }
      if (!done) genes.clear()
    }
  }

  def this(id: Int, xFile: Iterator[String]) {
    this(id)

    var done = false
    while (!done && xFile.hasNext) {
      val xline = xFile.next()

      val st = xline.split("\\s+")
      st(0) match {
        case "genomeend" =>
          if (st(1).toInt != genome_id) println(" *ERROR* id mismatch in genome")
          done = true
        case "trait" => traits += Trait(xline)
        case "node" => nodes += NNode(xline, traits)
        case "gene" => genes += Gene(xline, traits, nodes)
        case _ => // every thing else ignore, including comments
      }
    }
  }

  def findTraitIn(traitsArray: ArrayBuffer[Trait], toFind: Trait): Trait = {
    toFind match {
      case null => null
      case t: Trait => traitsArray.find(_.trait_id == t.trait_id).orNull
    }
  }

  def duplicate(new_id: Int): Genome = {
    // duplicate traits
    val traits_dup = traits map (Trait(_))

    // duplicate NNodes
    val nodes_dup = nodes map { _node =>
      val assoc_trait = findTraitIn(traits_dup, _node.nodetrait)
      _node.dup = NNode(_node, assoc_trait)
      _node.dup
    }

    // duplicate Genes
    val genes_dup = genes map { _gene =>
      // point to new nodes created  at precedent step
      val inode = _gene.lnk.in_node.dup
      val onode = _gene.lnk.out_node.dup
      val assoc_trait = findTraitIn(traits_dup, _gene.lnk.linktrait)

      // creation of new gene with a pointer to new node
      Gene(_gene, assoc_trait, inode, onode)
    }

    // okay all nodes created, the new genome can be generate
    new Genome(new_id, traits_dup, nodes_dup, genes_dup)
  }

  def mutate_link_weight(power: Double, rate: Double, mutation_type: Int): Unit = {
    //The power of mutation will rise farther into the genome
    //on the theory that the older genes are more fit since
    //they have stood the test of time

    //Once in a while really shake things up
    // for 50% of Prob, severe is true
    val severe = if (Util.randfloat > 0.5) true else false
    val gene_total = genes.size.toDouble

    //Decide what kind of mutation to do on a gene
    val endpart = gene_total * 0.8

    val powermod = 1.0
    var num = 0.0

    // depends on local variables in this function
    def calcGaussAndColdGaussPoint: (Double, Double) = {
      val (gausspoint, coldgausspoint) = if (severe) {
        (0.3, 0.1)
      }
      else { // with other 50%.....
        if (gene_total >= 10.0 && num > endpart)
          (0.5, 0.3)
        else if (Util.randfloat > 0.5)
          (1.0 - rate, 1.0 - rate - 0.1)
        else
          (1.0 - rate, 1.0 - rate)
      }

      (gausspoint, coldgausspoint)
    }

    for (_gene <- genes) {
      val (gausspoint, coldgausspoint) = calcGaussAndColdGaussPoint

      // choise a number from ]-1,+1[
      val randnum = Util.randposneg * Util.randfloat * power * powermod
      if (mutation_type == NeatConstant.GAUSSIAN) {
        val randchoice = Util.randfloat // a number from ]0,1[
        if (randchoice > gausspoint)
          _gene.lnk.weight += randnum
        else if (randchoice > coldgausspoint)
          _gene.lnk.weight = randnum
      }
      else if (mutation_type == NeatConstant.COLDGAUSSIAN)
        _gene.lnk.weight = randnum

      // copy to mutation_num, the current weight
      _gene.mutation_num = _gene.lnk.weight

      num += 1.0
    }
  }

  def genesis(id: Int): Network = {
    val all_list = nodes map { _node =>
      // create a copy of gene node for fenotype
      val newnode = new NNode(_node.node_type, _node.node_id, _node.gen_node_label)

      // Derive link's parameters from its Trait pointer of nodetrait
      newnode.derive_trait(_node.nodetrait)
      _node.analogue = newnode

      newnode
    }

    val inlist = all_list filter { _node =>
      _node.gen_node_label == NeatConstant.INPUT || _node.gen_node_label == NeatConstant.BIAS
    }

    val outlist = all_list filter { _node => _node.gen_node_label == NeatConstant.OUTPUT }

    require(genes.nonEmpty, "ALERT : are a network whitout GENES; the result can unpredictable")
    require(outlist.nonEmpty, "ALERT : are a network whitout OUTPUTS; the result can unpredictable")

    // Only create the link if the gene is enabled
    genes filter (_.enable == true) foreach { _gene =>
      val curlink = _gene.lnk
      val inode = curlink.in_node.analogue
      val onode = curlink.out_node.analogue

      //NOTE: This line could be run through a recurrency check if desired
      // (no need to in the current implementation of NEAT)
      val newlink = new Link(curlink.weight, inode, onode, curlink.is_recurrent)
      onode.incoming.append(newlink)
      inode.outgoing.append(newlink)

      // of linktrait
      val curtrait = curlink.linktrait
      curlink.derive_trait(curtrait)
    }

    // Create the new network
    val newnet = new Network(inlist, outlist, all_list, id)

    // Attach genotype and phenotype together:
    //  newnet point to owner genotype (this)
    newnet.genotype = this

    // genotype point to owner phenotype (newnet)
    phenotype = newnet
    newnet
  }

  /**
    * This function gives a measure of compatibility between
    * two Genomes by computing a linear combination of 3
    * characterizing variables of their compatibilty.
    * The 3 variables represent PERCENT DISJOINT GENES,
    * PERCENT EXCESS GENES, MUTATIONAL DIFFERENCE WITHIN
    * MATCHING GENES.  So the formula for compatibility
    * is:  disjoint_coeff*pdg+excess_coeff*peg+mutdiff_coeff*mdmg.
    * The 3 coefficients are global system parameters
    */
  def compatibility(g: Genome): Double = {
    // Set up the counters
    var num_disjoint = 0.0
    var num_excess = 0.0
    var mut_diff_total = 0.0
    var num_matching = 0.0

    val g1Iter = genes.iterator.buffered
    val g2Iter = g.genes.iterator.buffered

    // Now move through the Genes of each potential parent until both Genomes end
    while (g1Iter.hasNext || g2Iter.hasNext) {
      if (!g1Iter.hasNext) {
        num_excess += 1.0
        g2Iter.next()
      }
      else if (!g2Iter.hasNext) {
        num_excess += 1.0
        g1Iter.next()
      }
      else {
        val _gene1 = g1Iter.head
        val _gene2 = g2Iter.head

        // Extract current innovation numbers
        val p1innov = _gene1.innovation_num
        val p2innov = _gene2.innovation_num

        if (p1innov == p2innov) {
          num_matching += 1.0

          val mut_diff = Math.abs(_gene1.mutation_num - _gene2.mutation_num)
          mut_diff_total += mut_diff

          g1Iter.next()
          g2Iter.next()
        }
        else if (p1innov < p2innov) {
          num_disjoint += 1.0
          g1Iter.next()
        }
        else if (p2innov < p1innov) {
          num_disjoint += 1.0
          g2Iter.next()
        }
      }
    }

    // Return the compatibility number using compatibility formula
    // Note that mut_diff_total/num_matching gives the AVERAGE
    // difference between mutation_nums for any two matching Genes
    // in the Genome.
    // Look at disjointedness and excess in the absolute (ignoring size)
    Neat.p_disjoint_coeff * (num_disjoint / 1.0) +
      Neat.p_excess_coeff * (num_excess / 1.0) +
      Neat.p_mutdiff_coeff * (mut_diff_total / num_matching)
  }


  def get_last_gene_innovnum: Double = (genes.last).innovation_num + 1

  def get_last_node_id: Int = (nodes.last).node_id + 1

  def checkForDupeGenes(genesL1: ArrayBuffer[Gene], genesL2: ArrayBuffer[Gene]): Boolean = {
    if (genesL1.isEmpty || genesL1.tail.isEmpty) false
    else if (genesL2.isEmpty) checkForDupeGenes(genesL1.tail, genesL1.tail.tail)
    else if (genesL1.head.geneEquals(genesL2.head)) true
    else checkForDupeGenes(genesL1, genesL2.tail)
  }

  def verify: Boolean = {
    var disab = false
    var last_id = 0

    var valid = !(genes.isEmpty || nodes.isEmpty || traits.isEmpty)

    // control if nodes in gene are defined and are the same nodes il nodes list
    for (_gene <- genes; if valid == true) {
      val inode = _gene.lnk.in_node
      val onode = _gene.lnk.out_node

      if (inode == null) {
        println(" *ERROR* inode = null in genome #" + genome_id)
        valid = false
      }

      if (onode == null) {
        println(" *ERROR* onode = null in genome #" + genome_id)
        valid = false
      }

      if (!nodes.contains(inode)) {
        println("Missing inode:  node defined in gene not found in Vector nodes of genome #" + genome_id)
        print("\n the inode is=" + inode.node_id)
        valid = false
      }

      if (!nodes.contains(onode)) {
        println("Missing onode:  node defined in gene not found in Vector nodes of genome #" + genome_id)
        print("\n the onode is=" + onode.node_id)
        valid = false
      }
    }

    // verify if list nodes is ordered
    for (_node <- nodes; if valid == true) {
      if (_node.node_id < last_id) {
        println("ALERT: NODES OUT OF ORDER : ")
        println(" last node_id is= " + last_id + " , current node_id=" + _node.node_id)
        valid = false
      }
      last_id = _node.node_id
    }

    if (valid && genes.nonEmpty) {
      if (checkForDupeGenes(genes, genes.tail)) {
        print(" \n  ALERT: DUPLICATE GENES :")
        valid = false
      }
    }

    if (nodes.size >= 500) {
      disab = false
      for (_gene <- genes) {
        if (!_gene.enable && disab) {
          print("\n ALERT: 2 DISABLES IN A ROW: " + _gene.lnk.in_node.node_id)
          print(" inp node=" + _gene.lnk.in_node.node_id)
          print(" out node=" + _gene.lnk.out_node.node_id)
          print(" for GENOME " + genome_id)
          print("\n Gene is :")
          _gene.op_view()
        }

        if (!_gene.enable)
          disab = true
        else
          disab = false
      }
    }
    valid
  }

  def node_insert(nlist: ArrayBuffer[NNode], n: NNode) = {
    val id = n.node_id
    val j = nlist.indexWhere(nTmp => nTmp.node_id >= id)
    if (j > 0) nlist.insert(j, n) else nlist.append(n)
  }

  def node_create(from_node: NNode, newtraits: ArrayBuffer[Trait], first_traitnum: Int): NNode = {
    val nodetraitnum = if (from_node.nodetrait == null) 0 else from_node.nodetrait.trait_id - first_traitnum
    val newtrait = newtraits(nodetraitnum)
    NNode(from_node, newtrait)
  }

  def gene_create(from_gene: Gene, first_traitnum: Int,
                  newnodes: ArrayBuffer[NNode], newtraits: ArrayBuffer[Trait]): Gene = {
    // Now add the chosengene to the baby
    // Next check for the nodes, add them if not in the baby Genome already
    val inode = from_gene.lnk.in_node
    val onode = from_gene.lnk.out_node

    // Check for inode, onode in the newnodes list
    val new_inode_opt = newnodes.find(_.node_id == inode.node_id)
    val new_onode_opt = newnodes.find(_.node_id == onode.node_id)

    // get or create new nodes
    val new_inode = new_inode_opt.getOrElse(node_create(inode, newtraits, first_traitnum))
    val new_onode = new_onode_opt.getOrElse(node_create(onode, newtraits, first_traitnum))

    // if new nodes are created, then add to newnodes, order is important
    if (inode.node_id < onode.node_id) {
      if (new_inode_opt.isEmpty) node_insert(newnodes, new_inode)
      if (new_onode_opt.isEmpty) node_insert(newnodes, new_onode)
    }
    else {
      if (new_onode_opt.isEmpty) node_insert(newnodes, new_onode)
      if (new_inode_opt.isEmpty) node_insert(newnodes, new_inode)
    }

    // Add the Gene
    val traitnum = if (from_gene.lnk.linktrait == null)
      first_traitnum
    else
      from_gene.lnk.linktrait.trait_id - first_traitnum
    val newtrait = newtraits(traitnum)

    Gene(from_gene, newtrait, new_inode, new_onode)
  }

  // Check to see if the chosengene conflicts with an already chosen gene
  // i.e. do they represent the same link
  def check_gene_conflict(gene_to_check: Gene, newgenes: ArrayBuffer[Gene]): Boolean = {
    newgenes exists { _curgene =>
      val check1 = _curgene.lnk.in_node.node_id == gene_to_check.lnk.in_node.node_id &&
        _curgene.lnk.out_node.node_id == gene_to_check.lnk.out_node.node_id &&
        _curgene.lnk.is_recurrent == gene_to_check.lnk.is_recurrent

      val check2 = _curgene.lnk.in_node.node_id == gene_to_check.lnk.out_node.node_id &&
        _curgene.lnk.out_node.node_id == gene_to_check.lnk.in_node.node_id &&
        !_curgene.lnk.is_recurrent && !gene_to_check.lnk.is_recurrent

      check1 || check2
    }
  }


  def mate_multipoint(g: Genome, genomeid: Int, fitness1: Double, fitness2: Double)(mateFunc: (Gene, Gene) => Gene) : Genome = {
    var disable = false
    var chosengene: Gene = null

    // Tells if the first genome (this one) has better fitness or not
    var skip = false

    // First, average the Traits from the 2 parents to form the baby's Traits
    // It is assumed that trait vectors are the same length
    // In the future, may decide on a different method for trait mating
    val newtraits = traits.zip(g.traits) map { case (_trait1, _trait2) => Trait(_trait1, _trait2) }

    // Figure out which genome is better
    // The worse genome should not be allowed to add extra structural baggage
    // If they are the same, use the smaller one's disjoint and excess genes only
    val p1better = (fitness1 > fitness2) || (fitness1 == fitness2 && genes.size < g.genes.size)

    val newgenes = ArrayBuffer[Gene]()
    val newnodes = ArrayBuffer[NNode]()

    val g1Iter = genes.iterator.buffered
    val g2Iter = g.genes.iterator.buffered

    while (g1Iter.hasNext || g2Iter.hasNext) {
      //  chosen of 'just' gene
      skip = false //Default to not skipping a chosen gene
      if (!g1Iter.hasNext) {
        chosengene = g2Iter.next()
        if (p1better) skip = true //Skip excess from the worse genome
      }
      else if (!g2Iter.hasNext) {
        chosengene = g1Iter.next()
        if (!p1better) skip = true
      }
      else {
        val _p1gene = g1Iter.head
        val _p2gene = g2Iter.head

        val p1innov = _p1gene.innovation_num
        val p2innov = _p2gene.innovation_num

        if (p1innov == p2innov) {
          chosengene = mateFunc(_p1gene, _p2gene)

          // If one is disabled, the corresponding gene in the offspring
          // will likely be disabled
          disable = false
          if (_p1gene.enable == false || _p2gene.enable == false) {
            if (Util.randfloat < 0.75) {
              disable = true
            }
          }
          g1Iter.next()
          g2Iter.next()
        }
        else if (p1innov < p2innov) {
          chosengene = _p1gene
          g1Iter.next()
          if (!p1better) skip = true
        }
        else if (p2innov < p1innov) {
          chosengene = _p2gene
          g2Iter.next()
          if (p1better) skip = true
        }
      }

      if (!skip && !check_gene_conflict(chosengene, newgenes)) {
        // Now add the chosengene to the baby
        val first_traitnum = traits.head.trait_id
        val newgene = gene_create(chosengene, first_traitnum, newnodes, newtraits)
        if (disable) {
          newgene.enable = false
          disable = false
        }
        newgenes.append(newgene)
      }
      // end block genome (while)
    }

    new Genome(genomeid, newtraits, newnodes, newgenes)
  }

  def mate_multipoint_random(g: Genome, genomeid: Int, fitness1: Double, fitness2: Double): Genome = {
    val new_genome = mate_multipoint(g, genomeid, fitness1, fitness2) { (_p1gene, _p2gene) =>
      if (Util.randfloat < 0.5) _p1gene else _p2gene
    }

    // ----------------------------------------------------------------------------------------
    //	boolean h = new_genome.verify();
    val foundBool = new_genome.nodes.exists(_.gen_node_label == NeatConstant.OUTPUT)
    if (!foundBool) {
      print("\n *--------------- not found output node ----------------------------")
      print("\n * during mate_multipoint : please control the following's *********")
      print("\n * control block : ")
      print("\n Genome A= ")
      this.op_view()
      print("\n Genome B= ")
      g.op_view()
      print("\n Result = ")
      new_genome.op_view()
      System.exit(0)
    }

    new_genome
  }

  def op_view(): Unit = {
    print("\n GENOME START   id=" + genome_id)
    print("\n  genes are :" + genes.size)
    print("\n  nodes are :" + nodes.size)
    print("\n  trait are :" + traits.size)

    for (_node <- nodes) {
      _node.gen_node_label match {
        case NeatConstant.INPUT => print("\n Input ")
        case NeatConstant.OUTPUT => print("\n Output")
        case NeatConstant.HIDDEN => print("\n Hidden")
        case NeatConstant.BIAS => print("\n Bias  ")
      }
      _node.op_view()
    }

    genes foreach (_.op_view())

    print("\n")
    print(" Traits:\n")

    traits foreach (_.op_view())

    print("\n")
    print(" GENOME END")
  }

  def mate_multipoint_avg(g: Genome, genomeid: Int, fitness1: Double, fitness2: Double): Genome = {
    mate_multipoint(g, genomeid, fitness1, fitness2) { (_p1gene, _p2gene) =>
      Gene.create_avg_gene(_p1gene, _p2gene)
    }
  }

  def mate_singlepoint(g: Genome, genomeid: Int): Genome = {
    val newtraits = traits.zip(g.traits) map { case (_trait1, _trait2) => Trait(_trait1, _trait2) }
    val newgenes = ArrayBuffer[Gene]()
    val newnodes = ArrayBuffer[NNode]()

    val (genomeA, genomeB) = if (genes.size < g.genes.size) (genes, g.genes) else (g.genes, genes)

    val crosspoint = Util.randint(0, genomeA.size - 1)

    // print("\n crossing point is :"+crosspoint);
    var genecounter = 0

    val gAIter = genomeA.iterator.buffered
    val gBIter = genomeB.iterator.buffered

    var geneA : Gene = null
    var geneB : Gene = null

    // compute what is the highest innovation
    val last_innovB = genomeB(genomeB.size - 1).innovation_num
    var cross_innov = 0.0
    while (gAIter.hasNext || gBIter.hasNext) {
      var skip = false
      var p1p2comp = 0

      if (!gAIter.hasNext) {
        p1p2comp = 1
        geneB = gBIter.next()
      }
      else if (!gBIter.hasNext) {
        p1p2comp = -1
        geneA = gAIter.next()
      }
      else {
        geneA = gAIter.head
        geneB = gBIter.head

        val p1innov = geneA.innovation_num
        val p2innov = geneB.innovation_num

        if (p1innov < p2innov) {
          p1p2comp = -1
          gAIter.next()
        }
        else if (p1innov == p2innov) {
          p1p2comp = 0
          gAIter.next()
          gBIter.next()
        }
        else {
          p1p2comp = 1
          gBIter.next()
        }
      }

      var chosengene: Gene = null

      //   innovA = innovB
      if (p1p2comp == 0) {
        if (genecounter < crosspoint) {
          chosengene = geneA
        }
        else if (genecounter == crosspoint) {
          val avgene = Gene.create_avg_gene(geneA, geneB)
          if ((geneA.enable == false) || (geneB.enable == false)) avgene.enable = false

          chosengene = avgene
          cross_innov = geneA.innovation_num
        }
        else if (genecounter > crosspoint) {
          chosengene = geneB
        }
      }
      //   innovA < innovB
      else if (p1p2comp < 0) {
        if (genecounter < crosspoint) {
          chosengene = geneA //make geneA
        }
        else if (genecounter == crosspoint) {
          chosengene = geneA
          cross_innov = geneA.innovation_num
        }
        else if (genecounter > crosspoint)
          if (cross_innov > last_innovB) {
            chosengene = geneA
          }
          else
            skip = true
      }
      //   innovA > innovB
      else if (p1p2comp > 0) {
          if (genecounter < crosspoint)
            skip = true // skip geneB
          else if (genecounter == crosspoint)
            skip = true // skip an illogic case
          else if (genecounter > crosspoint) {
            chosengene = if (cross_innov > last_innovB) geneA else geneB
          }
      }

      if (!skip)
        genecounter += 1

      if (!skip && !check_gene_conflict(chosengene, newgenes)) {
        // Now add the chosengene to the baby
        val first_traitnum = traits.head.trait_id
        val newgene = gene_create(chosengene, first_traitnum, newnodes, newtraits)
        newgenes.append(newgene)
      }
    }

    new Genome(genomeid, newtraits, newnodes, newgenes)
  }

  def mutate_gene_reenable() = {
    // find the first disabled gene and enable it
    genes.find(_.enable == false).foreach(gene => gene.enable = true)
  }

  /**
    * This chooses a random gene, extracts the link from it,
    * and repoints the link to a random trait
    */
  def mutate_link_trait(times: Int) = {
    for (_ <- 1 to times) {
      // choose a random gene
      val _gene = Util.getRandomEntry(genes)

      // set the link to point to a random trait
      _gene.lnk.linktrait = Util.getRandomEntry(traits)
    }
  }

  /**
    * This chooses a random node
    * and repoints the node to a random trait
    */
  def mutate_node_trait(times: Int) = {
    for (_ <- 1 to times) {
      // Choose a random node
      val _node = Util.getRandomEntry(nodes)

      // choose a random trait
      _node.nodetrait = Util.getRandomEntry(traits)
    }
  }

  def mutate_random_trait() = {
    // get a random trait and mutate it
    val traitRand = Util.getRandomEntry(traits)
    traitRand.mutate()
  }

  // Toggle genes from enable on to enable off or
  //   vice versa.  Do it times times.
  def mutate_toggle_enable(times: Int) = {
    for (_ <- 1 to times) {
      // Choose a random genenum
      val _gene = Util.getRandomEntry(genes)

      // Toggle the enable on this gene
      if (_gene.enable) {
        // We need to make sure that another gene connects out of the in-node
        // Because if not a section of network will break off and become isolated
        val found = genes exists { _jgene =>
          (_gene.lnk.in_node eq _jgene.lnk.in_node) &&
            _jgene.enable &&
            (_jgene.innovation_num != _gene.innovation_num)
        }

        // Disable the gene if it's safe to do so
        if (found)
          _gene.enable = false
      }
      else
        _gene.enable = true
    }
  }

  def mutate_add_link(pop: Population, tries: Int): Boolean = {
    val thresh = nodes.size * nodes.size
    var nodenum1 = 0
    var nodenum2 = 0
    var thenode1: NNode = null
    var thenode2: NNode = null

    // Decide whether to make this recurrent
    val do_recur = Util.randfloat < Neat.p_recur_only_prob

    // Find the first non-sensor so that the to-node won't look at sensors as
    // possible destinations
    val first_nonsensor = nodes.indexWhere(_.node_type != NeatConstant.SENSOR)

    var found = false
    for (_ <- 0 until tries if !found) {
      // at this point, in case of recurrence :
      // 50% of prob to decide a loop recurrency( node X to node X)
      // 50% a normal recurrency ( node X to node Y)
      if (do_recur && Util.randfloat > 0.5) {
        nodenum1 = Util.randint(first_nonsensor, nodes.size - 1)
        nodenum2 = nodenum1
      }
      else {
        nodenum1 = Util.randint(0, nodes.size - 1)
        nodenum2 = Util.randint(first_nonsensor, nodes.size - 1)
      }

      // now point to object's nodes
      thenode1 = nodes(nodenum1)
      thenode2 = nodes(nodenum2)

      // verify if the possible new gene already EXIST
      val bypass = genes exists { _gene =>
        val g_in_node = _gene.lnk.in_node
        val g_out_node = _gene.lnk.out_node
        val g_is_recurrent = _gene.lnk.is_recurrent

        (thenode2.node_type == NeatConstant.SENSOR) ||
          ((g_in_node eq thenode1) && (g_out_node eq thenode2) && g_is_recurrent && do_recur) ||
          ((g_in_node eq thenode1) && (g_out_node eq thenode2) && !g_is_recurrent && !do_recur)
      }

      if (!bypass) {
        phenotype.status = 0
        val recurflag = phenotype.has_a_path(thenode1.analogue, thenode2.analogue, 0, thresh)
        if (phenotype.status == 8) {
          println("\n  network.mutate_add_link : LOOP DETECTED DURING A RECURRENCY CHECK")
          return false
        }

        // TODO: simplyfy this condition - should it just be !recurflag && do_recur?
        if ((!recurflag && do_recur) || (recurflag && !do_recur)) {
          // ignore this condition
        }
        else {
          found = true
        }
      }
    }

    if (found) {
      //If the phenotype does not exist, exit on false,print error
      //Note: This should never happen- if it does there is a bug
      if (phenotype == null) {
        print("ERROR: Attempt to add link to genome with no phenotype")
        return false
      }

      // Check to see if this innovation already occured in the population
      val innov_opt = pop.innovations filter (_.isInstanceOf[LinkInnovation]) find { _innov =>
        (_innov.node_in_id == thenode1.node_id) &&
          (_innov.node_out_id == thenode2.node_id) &&
          (_innov.recur_flag == do_recur)
      }

      val new_gene = if (innov_opt.isDefined) {
        val _innov: Innovation = innov_opt.get
        Gene(traits(_innov.new_traitnum), _innov.new_weight, thenode1, thenode2, do_recur, _innov.innovation_num1, 0)
      }
      else {
        // Choose a random trait
        val traitnum = Util.randint(0, traits.size - 1)

        // choose the new weight
        // newweight=(gaussrand())/1.5;  //Could use a gaussian
        val new_weight = Util.randposneg * Util.randfloat * 10.0

        // read from population current innovation value
        // read curr innovation with postincrement
        val curr_innov = pop.getCurr_innov_num_and_increment

        // create the new gene
        val tmp_gene = Gene(traits(traitnum), new_weight, thenode1, thenode2, do_recur, curr_innov, new_weight)

        // add the innovation
        pop.innovations.append(new LinkInnovation(thenode1.node_id, thenode2.node_id, curr_innov, new_weight, traitnum))
        tmp_gene
      }

      genes.append(new_gene)
      return true
    }

    false
  }

  def mutate_add_node(pop: Population): Boolean = {

    val foundGene = if (genes.size < 15) {
      genes dropWhile { _gene =>
        (!_gene.enable) || _gene.lnk.in_node.gen_node_label == NeatConstant.BIAS
      } find { _gene =>
        (Util.randfloat >= 0.3) && (_gene.lnk.in_node.gen_node_label != NeatConstant.BIAS)
      }
    }
    else {
      // random splitting, try upto 20 times till an enabled non-bias gene is found
      (0 until 20) map (_ => Util.getRandomEntry(genes)) find { _gene =>
        _gene.enable && (_gene.lnk.in_node.gen_node_label != NeatConstant.BIAS)
      }
    }

    if (foundGene.isEmpty) {
      false
    }
    else {
      var newgene1: Gene = null
      var newgene2: Gene = null
      var new_node: NNode = null

      val _gene = foundGene.get
      _gene.enable = false

      // Extract the link
      val thelink = _gene.lnk

      // Extract the weight;
      val oldweight = thelink.weight

      // Get the old link's trait
      val traitptr = thelink.linktrait

      // Extract the nodes
      val in_node = thelink.in_node
      val out_node = thelink.out_node

      val innov_opt = pop.innovations filter (_.isInstanceOf[NodeInnovation]) find { _innov =>
        (_innov.node_in_id == in_node.node_id) &&
          (_innov.node_out_id == out_node.node_id) &&
          (_innov.old_innov_num == _gene.innovation_num)
      }

      if (innov_opt.isDefined) {
        val _innov = innov_opt.get

        // Create the new Genes
        // pass this current nodeid to newnode
        new_node = new NNode(NeatConstant.NEURON, _innov.newnode_id, NeatConstant.HIDDEN, traits.head)

        newgene1 = Gene(traitptr, 1.0, in_node, new_node, thelink.is_recurrent, _innov.innovation_num1, 0)
        newgene2 = Gene(traitptr, oldweight, new_node, out_node, false, _innov.innovation_num2, 0)
      }
      else {
        // The innovation is totally novel
        // Create the new Genes and new NNode
        // By convention, it will point to the first trait

        // get the current node id with postincrement
        val curnode_id = pop.getCur_node_id_and_increment

        // pass this current nodeid to newnode and create the new node
        new_node = new NNode(NeatConstant.NEURON, curnode_id, NeatConstant.HIDDEN, traits.head)

        // get the current gene inovation with post increment
        val gene_innov1 = pop.getCurr_innov_num_and_increment

        // create gene with the current gene inovation
        newgene1 = Gene(traitptr, 1.0, in_node, new_node, thelink.is_recurrent, gene_innov1, 0)

        // re-read the current innovation with increment
        val gene_innov2 = pop.getCurr_innov_num_and_increment

        // create the second gene with this innovation incremented
        newgene2 = Gene(traitptr, oldweight, new_node, out_node, false, gene_innov2, 0)
        pop.innovations.append(new NodeInnovation(in_node.node_id, out_node.node_id, gene_innov1, gene_innov2, new_node.node_id, _gene.innovation_num))
      }

      // Now add the new NNode and new Genes to the Genome
      genes.append(newgene1)
      genes.append(newgene2)
      node_insert(nodes, new_node)
      true
    }
  }

}

object Genome {
  def print_to_filename(genome: Genome, xNameFile: String): Unit = {
    // write to file genome in native format (for re-read)
    Util.writeToFile(xNameFile) { printWriter =>
      print_to_file(genome, printWriter)
    }
  }

  def print_to_file(genome: Genome, printWriter: PrintWriter): Unit = {
    // write to file genome in native format (for re-read)
    printWriter.println("genomestart  " + genome.genome_id)

    genome.traits.foreach(t => Trait.print_to_file(t, printWriter))
    genome.nodes.foreach(n => NNode.print_to_file(n, printWriter))
    genome.genes.foreach(g => Gene.print_to_file(g, printWriter))

    printWriter.println("genomeend " + genome.genome_id)
  }
}