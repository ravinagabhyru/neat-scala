# neat-scala: NeuroEvolution of Augmenting Topologies in Scala

neat-scala is based on the NeuroEvolution of Augmenting Topologies
method of evolving artificial neural networks as described by Stanley and
Miikkulainen (2001).

More information can be found on  [NEAT Users Page](http://www.cs.ucf.edu/~kstanley/neat.html)

This implementation is based on [JNEAT](http://nn.cs.utexas.edu/soft-view.php?SoftID=5)
 (java version of neat). Original Java version was converted to Scala (using Java to Scala code
converter in Intellij) and then changed to use Scala idioms. There is
no GUI in this version. It is command line based and written as an API
for use in other projects.

There is one example currently, implementaion of XOR. I plan to add more
examples in future.

I also plan to refactor the code to be able to run in parallel, simplify
the interface, provide callbacks to save state, better logging and maybe
implement HyperNEAT or other advances in NEAT.

## Dependencies

Currently, the only dependencies are sbt v0.13.15 (to compile and run
examples) and scala v2.11.8 (any version 2.11 or higher should work).

## Getting Started

Clone this repository and compile in sbt and execute the XOR example


```
git clone https://github.com/ravinagabhyru/neat-scala.git 
sbt compile
sbt examples/run
```

XOR example expects "data/parameters" file in the project directory
and writes output to the same directory. I will have to change this
and make it a command line option.

## Resources

- [Kenneth Stanley's Web Site](http://www.cs.ucf.edu/~kstanley/)
- [JNEAT - Java version of NEAT](http://nn.cs.utexas.edu/soft-view.php?SoftID=5)
- [Neat Python](https://github.com/CodeReclaimers/neat-python)
