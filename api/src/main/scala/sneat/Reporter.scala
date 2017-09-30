package sneat

trait Reporter {
  def startGeneration(pop: Population, generation: Int) = ()
  def endGeneration(pop: Population, generation: Int) = ()
  def afterEvaluate(pop: Population, generation: Int) = ()
}
