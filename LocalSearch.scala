import Params._

object LocalSearchMain {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)

		val mejor = solver.solve
	
		val ls = new LocalSearch(inst, mejor)
		ls.relocate()
	}
}

class LocalSearch(inst: Instance, sol: List[List[Customer]]) {
	val rnd = new scala.util.Random()

	def sumd(l: List[Customer]): Double = {
		l.zip(l.tail).foldLeft(0.0)((x, y) => x + inst.distancia(y._1, y._2))
	}

	def relocate() = {
		def relocateCamion(camion: List[Customer]) = {
			def relocateCustomer(camion: List[Customer], customer: Customer) = {
				println("relocate de " + customer.num)

				println("camion original = " + camion.map(_.num))
				println("largo = " + sumd(camion))

				val camionIt = camion - customer
				println("camion = " + camionIt.map(_.num))
		
				for(i <- 1 to camionIt.length) {
					val l = camionIt.take(i) ::: List(customer) ::: camionIt.drop(i)
					if (l != camion && inst.camionFactible(l)) {
						println("new factible: " + l.map(_.num))
						println("new largo = " + sumd(l))
					}
				}
				println("")
			}

			camion.tail.foreach(relocateCustomer(camion, _))
		}

		sol.foreach(relocateCamion _)
	}	
}
