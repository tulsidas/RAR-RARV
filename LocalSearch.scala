import Params._

object LocalSearchMain {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)

		val mejor = solver.solve
	
		val ls = new LocalSearch(inst, mejor)
		ls.dosOpt()		

		/*	
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
		*/
	}
}

class LocalSearch(inst: Instance, solucion: List[List[Customer]]) {
	val rnd = new scala.util.Random()
	type Ruta = List[Customer]

	private def sumd(l: Ruta): Double = {
		l.zip(l.tail).foldLeft(0.0)((x, y) => x + inst.distancia(y._1, y._2))
	}

	private def relocateOpt(n: Int)(camion: Ruta): List[Ruta] = {
		val ret = new scala.collection.mutable.ListBuffer[Ruta]
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

	private def reverseOpt(n: Int)(camion: Ruta): List[Ruta] = {
		def _swap(_camion: Ruta): List[Ruta] = {
			_camion match {
				case xs if xs.length < n => Nil
				case xs => List(xs.take(n).reverse ++ xs.drop(n)) ++ reverseOpt(n)(xs)
			}
		}
		
		// saco el source y lo agrego al resto
		_swap(camion.tail).map(camion.head :: _)		
	}
	
	private def search(gen: Ruta => List[Ruta]) = {
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
						//Imaginario.writeImage(i+".jpg", inst, List(camion))
						//Imaginario.writeImage(i+"_new.jpg", inst, List(l))
						println()
					}
				}
			}
		}
	}
	
	// multiruta
	private def crossExchange(n: Int)(l1: Ruta, l2: Ruta): List[(Ruta, Ruta)] = {
		val ret = new scala.collection.mutable.ListBuffer[(Ruta, Ruta)]
		
		// desde 1 para no cambiar el source
		for (i <- 1 to l1.length-n; j <- 1 to l2.length-n) {

			// cambio el cachito de l1(i) en l2(j)
			val nl1 = l1.take(i) ++ l2.drop(j).take(n) ++ l1.drop(i+n)
			val nl2 = l2.take(j) ++ l1.drop(i).take(n) ++ l2.drop(j+n)

			ret += ((nl1, nl2))
		}
		
		ret.toList
	}
	
	private def tailExchange(l1: Ruta, l2: Ruta): List[(Ruta, Ruta)] = {
		val ret = new scala.collection.mutable.ListBuffer[(Ruta, Ruta)]
		for (i <- 1 to l1.length/*-1*/; j <- 1 to l2.length/*-1*/ if (!(i == 1 && j == 1))) {
			ret += ((l1.take(j) ++ l2.drop(i), l2.take(i) ++ l1.drop(j)))
		}
	
		ret.toList
	}
	
	private def multisearch(gen: (Ruta, Ruta) => List[(Ruta, Ruta)]) = {
		def producto(l: List[Ruta]): List[(Ruta, Ruta)] = {
			val ret = new scala.collection.mutable.ListBuffer[(Ruta, Ruta)]
			for (i <- 0 to l.length-1; j <- i+1 to l.length-1) {
				ret += ((l(i), l(j)))
			}
			ret.toList
		}
		
		var i = 0

		// agarro cada par de rutas y pruebo hacer cambios
		producto(solucion).foreach { par =>
			val l1 = par._1
			val l2 = par._2

			gen(l1, l2).foreach { np =>
				// por cada par, me fijo que las nuevas sean factibles y de menor largo
				val n1 = np._1
				val n2 = np._2
				
				if (inst.camionFactible(n1) && inst.camionFactible(n2)) {
					// cambio factible
					if (sumd(n1) + sumd(n2) < sumd(l1) + sumd(l2)) {
						println("cambio mejorable")
						println(l1.map(_.num))
						println(l2.map(_.num))
						println("largo = " + (sumd(l1) + sumd(l2)))
						println("=>")
						println(n1.map(_.num))
						println(n2.map(_.num))
						println("largo = " + (sumd(n1) + sumd(n2)))
						println("----------------")
						Imaginario.writeImage(i+".jpg", inst, List(l1, l2))
						Imaginario.writeImage(i+"_new.jpg", inst, List(n1, n2))
						i = i+1
					}
				}
			}
		}
	}
	
	// metodos API
	// intraruta
	def reverse(n: Int) = search(reverseOpt(n))
	def relocate(n: Int) = search(relocateOpt(n))
	
	// def interruta
	def dosOpt() = multisearch(tailExchange)
	def cross(n: Int) = multisearch(crossExchange(n))
}
