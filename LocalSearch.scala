import Params._

object LocalSearchMain {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)

		val mejor = solver.solve
	
		val ls = new LocalSearch(inst, mejor)
		//ls.relocate()
		println("dosOpt")
		ls.dosOpt()
		println("tresOpt")
		ls.tresOpt()
		println("cuatroOpt")
		ls.cuatroOpt()
	}
}

class LocalSearch(inst: Instance, solucion: List[List[Customer]]) {
	val rnd = new scala.util.Random()

	def sumd(l: List[Customer]): Double = {
		l.zip(l.tail).foldLeft(0.0)((x, y) => x + inst.distancia(y._1, y._2))
	}

	def relocate() = {
		def relocateCamion(camion: List[Customer]) = {
			def relocateCustomer(camion: List[Customer], customer: Customer) = {
				val camionIt = camion - customer
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

		solucion.foreach(relocateCamion _)
	}

	def opt(n: Int)(camion: List[Customer]): List[List[Customer]] = {
		def _opt(_camion: List[Customer]): List[List[Customer]] = {
			_camion match {
				case xs if xs.length < n => Nil
				case xs => List(xs.take(n).reverse ++ xs.drop(n)) ++ opt(n)(xs)
			}
		}
		
		// saco el source y lo agrego al resto
		_opt(camion.tail).map(camion.head :: _)
	}
	
	def search(gen: List[Customer] => List[List[Customer]]) = {
		solucion.foreach { camion => 		
			val largo = sumd(camion)

			gen(camion).foreach { l =>
				if (inst.camionFactible(l)) {
					val newLargo = sumd(l)
					if (newLargo < largo) {
						println("anterior = " + camion.map(_.num) + "|" + sumd(camion))
						println("mejor solucion" + l.map(_.num) + "|" + sumd(l))
						println()
					}
				}
			}
		}
	}
	
	def dosOpt() = search(opt(2))
	def tresOpt() = search(opt(3))
	def cuatroOpt() = search(opt(4))
}
