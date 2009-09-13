import Params._

object LocalSearchMain {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)

		val mejor = solver.solve
	
		val ls = new LocalSearch(inst, mejor)
		//ls.relocate()
		ls.dosOpt()
		ls.tresOpt()
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

	def swap2(camion: List[Customer]): List[List[Customer]] = {
		camion match {
			case Nil => Nil
			case x :: Nil => Nil
			case x :: y :: xs => List(y :: x :: xs) ++ swap2(y :: xs).map(x :: _)
		}
	}
	
	def swap3(camion: List[Customer]): List[List[Customer]] = {
		camion match {
			case Nil => Nil
			case x :: Nil => Nil
			case x :: y :: Nil => Nil
			case x :: y :: z :: xs => List(z :: y :: x :: xs) ++ 
				swap3(y :: z :: xs).map(x :: _)
		}
	}

	def swap4(camion: List[Customer]): List[List[Customer]] = {
		camion match {
			case Nil => Nil
			case x :: Nil => Nil
			case x :: y :: Nil => Nil
			case x :: y :: z :: Nil => Nil
			case w :: x :: y :: z :: xs => List(z :: y :: x :: w :: xs) ++ 
				swap4(x :: y :: z :: xs).map(w :: _)
		}
	}
	
	def opt(swapper: List[Customer] => List[List[Customer]]) = {
		solucion.foreach { camion => 
			println("*"+camion.map(_ num))
			
			swapper(camion.tail).foreach { swapped =>
				val l = camion.head :: swapped
				println(" "+l.map(_ num))

				if (inst.camionFactible(l)) {
					println("new factible: " + l.map(_.num))
					println("new largo = " + sumd(l))
				}
			}
		}
	}
	
	def dosOpt() = opt(swap2)
	def tresOpt() = opt(swap3)
	def cuatroOpt() = opt(swap4)
}
