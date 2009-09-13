import Params._

object LocalSearchMain {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)

		val mejor = solver.solve
	
		val ls = new LocalSearch(inst, mejor)
		//ls.relocate()

		println("dosOpt********************************\n")
		ls.dosOpt()
		println("tresOpt********************************\n")
		ls.tresOpt()
		println("cuatroOpt********************************\n")
		ls.cuatroOpt()
		
		println("dosOrOpt********************************\n")
		ls.dosOrOpt()
		println("tresOrOpt********************************\n")
		ls.tresOrOpt()
		println("cuatroOrOpt********************************\n")
		ls.cuatroOrOpt()
	}
}

class LocalSearch(inst: Instance, solucion: List[List[Customer]]) {
	val rnd = new scala.util.Random()

	private def sumd(l: List[Customer]): Double = {
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

	private def opt(n: Int)(camion: List[Customer]): List[List[Customer]] = {
		def _opt(_camion: List[Customer]): List[List[Customer]] = {
			_camion match {
				case xs if xs.length < n => Nil
				case xs => List(xs.take(n).reverse ++ xs.drop(n)) ++ opt(n)(xs)
			}
		}
		
		// saco el source y lo agrego al resto
		_opt(camion.tail).map(camion.head :: _)		
	}
	
	private def orOpt(n: Int)(camion: List[Customer]): List[List[Customer]] = {
		def _orOpt(_camion: List[Customer]): List[List[Customer]] = {
			_camion match {
				case xs if xs.length < n + 1 => Nil

				case xs => {
					val chunk = xs.take(n)
					val rest = xs.drop(n)
					val chr = chunk ++ rest.tail
				
					List(rest.head :: chr) ++ _orOpt(chr).map(rest.head :: _)
				}
			}
		}
		
		// saco el source y lo agrego al resto
		_orOpt(camion.tail).map(camion.head :: _)
	}
	
	private def search(gen: List[Customer] => List[List[Customer]]) = {
		solucion.foreach { camion => 		
			val largo = sumd(camion)
			//println("original: " + camion.map(_.num))

			gen(camion).foreach { l =>
				//println("variante: " + l.map(_.num))
				if (inst.camionFactible(l)) {
					//println("factible: " + l.map(_.num) + "|" + sumd(l))
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

	def dosOrOpt() = search(orOpt(2))
	def tresOrOpt() = search(orOpt(3))
	def cuatroOrOpt() = search(orOpt(4))
}
