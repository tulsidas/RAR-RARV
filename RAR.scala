import scala.util.Random
import Params._

object RARMain {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		
		new RAR(inst)		
	}
}

class RAR(inst: Instance) {
	type Camion = List[Customer]

	var nn = new LocalSearch(inst, new NearestNeighbour(inst).solve).search()
	//var nn = new NearestNeighbour(inst).solve
		
	println("NN = " + nn.map(_.map(_.num)))
	println(nn.length + " vehiculos | " + inst.solLength(nn))
	println()
	
	//val ryr = recreate(nn, ruin(10, inst.customers(44)))
	var factibles = 0
	
	//var tau = new scala.collection.mutable.HashMap[Customer, Int]()
	
	while (factibles < 100) {
		val ryr = recreate(nn, ruin(π))
		if (inst.factible(ryr) && (ryr.length < nn.length || inst.solLength(ryr) < inst.solLength(nn))) {
			factibles = factibles + 1
			println("factible! " + ryr.length + " vehiculos | " + inst.solLength(ryr))
			//println(ryr.map(_.map(_.num)))

			// un nuevo mejor
			nn = ryr
		}
	}
	
	//println("\n\n\n")
	//println("tauMap:")
	//println(tau.map( e => (e._1.num, e._2) ).toList)
	
	/** arruino random */
	private def ruin(prob: Double): List[Customer] = {
		val rnd = new Random()
		def ruinP(c: Customer): Boolean = {
			rnd.nextDouble > prob
		}

		/*val ruined = */inst.customers.tail.filter(ruinP)

		//ruined.foreach(nv => tau.put(nv, tau.getOrElseUpdate(nv, 0)+1))
		// println("ruin => " + ret.map(_.num))
		//ruined
	}
	
	/** arruino por distancia al source */
	private def ruin(dist: Int): List[Customer] = ruin(dist, inst.source)
		
	/** arruino por distancia a un customer dado */
	private def ruin(dist: Int, cust: Customer): List[Customer] = {
		println("ruin("+dist+","+cust.num+")")
	
		inst.customers.filter(
			c => inst.distancia(c, cust) < dist && inst.distancia(c, cust) > 0)
	}
	
	private def recreate(solucion: List[Camion], rotos: List[Customer]): List[Camion] = {
		val arruinado = solucion.map(_ -- rotos).filter(_.length > 1)
		new LocalInsert(inst, arruinado).insert(rotos, Map())
	}
}	
