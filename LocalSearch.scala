import Params._

object LocalSearchMain {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)

		val mejor = solver.solve
	
		val ls = new LocalSearch(inst, mejor)
		println("largo original = " + ls.mejorSolucion.foldLeft(0.0)(_ + ls.sumd(_)))
		
		ls.search()

		println(ls.mejorSolucion.map(_.map(_.num)))
		println("largo resultante = " + ls.mejorSolucion.foldLeft(0.0)(_ + ls.sumd(_)))
	}
}

class LocalSearch(inst: Instance, solucion: List[List[Customer]]) {
	val rnd = new scala.util.Random()
	type Ruta = List[Customer]
	var i = 0
	
	var mejorSolucion = solucion

	def sumd(l: Ruta): Double = {
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
	
	private def unisearch(gen: Ruta => List[Ruta]): Unit = {
		mejorSolucion.foreach { camion => 		
			val largo = sumd(camion)

			gen(camion).foreach { l =>
				if (inst.camionFactible(l) && sumd(l) < largo) {
					// actualizo mejor solucion
					mejorSolucion = l :: (mejorSolucion - camion)

					//println("nuevo mejor largo = " + mejorSolucion.foldLeft(0.0)(_ + sumd(_)))

					// llamada recursiva con la mejora
					return unisearch(gen)						
				}
			}
		}
		//println("mejor largo local = " + mejorSolucion.foldLeft(0.0)(_ + sumd(_)))
	}
	
	// multiruta
	private def crossExchange(n: Int, m: Int)(l1: Ruta, l2: Ruta): List[(Ruta, Ruta)] = {
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
	
	private def multisearch(gen: (Ruta, Ruta) => List[(Ruta, Ruta)]): Unit = {
		def producto(l: List[Ruta]): List[(Ruta, Ruta)] = {
			val ret = new scala.collection.mutable.ListBuffer[(Ruta, Ruta)]
			for (i <- 0 to l.length-1; j <- i+1 to l.length-1) {
				ret += ((l(i), l(j)))
			}
			ret.toList
		}
		
		// agarro cada par de rutas y pruebo hacer cambios
		producto(mejorSolucion).foreach { par =>
			val l1 = par._1
			val l2 = par._2

			gen(l1, l2).foreach { np =>
				// por cada par, me fijo que las nuevas sean factibles y de menor largo
				val n1 = np._1
				val n2 = np._2
				
				if (inst.camionFactible(n1) && inst.camionFactible(n2)) {
					// cambio factible
					if (sumd(n1) + sumd(n2) < sumd(l1) + sumd(l2)) {
						//Imaginario.writeImage(i+".jpg", inst, List(l1, l2))
						//Imaginario.writeImage(i+"_new.jpg", inst, List(n1, n2))
						//i = i+1

						// actualizo mejor solucion
						mejorSolucion = List(n1, n2) ::: (mejorSolucion -- List(l1, l2))

						//println("nuevo mejor largo = " + mejorSolucion.foldLeft(0.0)(_ + sumd(_)))

						// llamada recursiva con la mejora
						return multisearch(gen)
					}
				}
			}
		}
		//println("mejor largo local (multi) = " + mejorSolucion.foldLeft(0.0)(_ + sumd(_)))
	}
	
	//////////////////////
	// metodos publicos //
	//////////////////////
	
	def search(): Unit = {
		println("search")
		val largo = mejorSolucion.foldLeft(0.0)(_ + sumd(_))
		
		println("dosOpt()")
		dosOpt()
		
		for (n <- 0 to 3; m <- 0 to 3) {
			println("relocate("+n+","+m+")")
			relocate(n, m)
		}
		
		for (n <- 2 to 4 reverse) {
			println("reverse("+n+")")
			reverse(n)
		}

		for (n <- 1 to 4 reverse) {
			println("relocate("+n+")")
			relocate(n)
		}
		
		val newLargo = mejorSolucion.foldLeft(0.0)(_ + sumd(_))
		if (newLargo < largo) {
			// sigo
			search()
		}		
	}

	// intraruta
	def reverse(n: Int) = unisearch(reverseOpt(n))
	def relocate(n: Int) = unisearch(relocateOpt(n))
	
	// def interruta
	def dosOpt() = multisearch(tailExchange)
	def relocate(n: Int, m: Int) = multisearch(crossExchange(n, m))
}
