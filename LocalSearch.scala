import Params._

object LocalSearchMain {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)

		val mejor = solver.solve
	
		val ls = new LocalSearch(inst, mejor)
		println("relocate********************************\n")
		ls.relocate(1)
		println("relocate 2********************************\n")
		ls.relocate(2)
		println("relocate 3********************************\n")
		ls.relocate(3)
		println("relocate 4********************************\n")
		ls.relocate(4)
		
		println("reverse 2********************************\n")
		ls.reverse(2)
		println("reverse 3********************************\n")
		ls.reverse(3)
		println("reverse 4********************************\n")
		ls.reverse(4)
	}
}

class LocalSearch(inst: Instance, solucion: List[List[Customer]]) {
	val rnd = new scala.util.Random()

	private def sumd(l: List[Customer]): Double = {
		l.zip(l.tail).foldLeft(0.0)((x, y) => x + inst.distancia(y._1, y._2))
	}

	private def relocateOpt(n: Int)(camion: List[Customer]): List[List[Customer]] = {
		val ret = new scala.collection.mutable.HashSet[List[Customer]]
		for(i <- 1 to camion.length-n) { // no muevo el source
			val chunk = camion.slice(i, i+n)
			val camionIt = camion -- chunk
		
			for (j <- 1 to camionIt.length) { // no muevo el source
				val l = camionIt.take(j) ::: chunk ::: camionIt.drop(j)
				if (l != camion) {
					ret += l
				}
			}
		}
		
		ret.toList
	}

	private def reverseOpt(n: Int)(camion: List[Customer]): List[List[Customer]] = {
		def _swap(_camion: List[Customer]): List[List[Customer]] = {
			_camion match {
				case xs if xs.length < n => Nil
				case xs => List(xs.take(n).reverse ++ xs.drop(n)) ++ reverseOpt(n)(xs)
			}
		}
		
		// saco el source y lo agrego al resto
		_swap(camion.tail).map(camion.head :: _)		
	}
	
	/*
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
	*/
	
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
	
	def reverse(n: Int) = search(reverseOpt(n))
	def relocate(n: Int) = search(relocateOpt(n))
}
