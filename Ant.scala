import scala.util.Random
import scala.collection.immutable.TreeMap
import Params._

class Ant(val inst: Instance) extends Solver {
	val rnd = new Random()

	/**
	 * el valor heurístico de distancia entre nodo y v
	 */	
	def η(nodo: Customer, v: Customer): Double = {
		//inst.tau(nodo, v) * Math.pow((1.0/inst.distancia(nodo, v)), β)
		inst.tau(nodo, v) / inst.distancia(nodo, v)
	}

	def proximo(nodo: Customer, vecinos: List[Customer], 
					hora: Double, capacidad: Int): Customer = {
		println("-----------------\ndesde " + nodo.num)
					
		// filtro los que llego a tiempo y me alcanza la capacidad
		val insertables = vecinos.filter(vecino => insertable(nodo, vecino, hora, capacidad))
		println("insertables: " + insertables.map(_.num))
		
		val tau = insertables.foldLeft(Map[Customer, Double]()) { 
			(m, v) => m(v) = inst.tau(nodo, v)
		}
		println("tau("+nodo.num+") | " + tau.map( p => (p._1.num, p._2)).toList.filter(_._2 > 0))
		
		if (insertables.isEmpty) {
			null
		}
		else {
			// un mapa [Customer, Double] con (tau*η^β) precalculado
			val mapη = insertables.foldLeft(Map[Customer, Double]()) { 
				(m, v) => m(v) = η(nodo, v)
			}
			
			println("mapη = " + mapη.map( p => (p._1.num, p._2)).toList.filter(_._2 > 0 ))

			// obtengo el mayor η
			val values = mapη.values.toList
			val bestη = values.tail.foldLeft(values.head){ (a,b) => if (a > b) a else b }
			val bests = insertables.filter(mapη(_) == bestη)

			val q = rnd.nextDouble

			if (q < q0) {
				// exploitation
				
				println("exploitation")

				// si hay varios con el mismo η, obtengo el que cierra antes
				if (bests.length > 1) {
					val best = bests.tail.foldLeft(bests.head) { (a,b) => if (a.due < b.due) a else b }
					println("best = " + best.num)
					best
				}
				else {
					println("bests.head = " + bests.head.num)
					bests.head
				}
			}
			else {
				// exploration
				println("exploration")

				// el denominador
				val Σ = mapη.values.reduceLeft(_+_)
				
				// construyo una lista de (Customer, proba)
				val probas = (mapη.map(t => (t._1, mapη(t._1) / Σ)) toList)// filter(_._2 > 0)
				
				println("probas = " + probas.map( p => (p._1.num, p._2)) )

				val ret = weightedCases(probas)
				
				println("elijo: " + ret.num)
				
				ret
			}
		}
	}

	def weightedCases[A](inp: List[(A, Double)]): A = {
		def coinFlip[A](p: Double)(a1: A, a2: A) = {
			if (rnd.nextDouble() < p) a1 else a2
		}

		def coinFlips[A](w: Double)(l: List[(A,Double)]): A = {
			l match {
				case Nil => error("no coinFlips")
				case (d, _) :: Nil => d
				case (d, p) :: rest => coinFlip(p/(1.0-w))(d, coinFlips(w+p)(rest))
			}
		}

		coinFlips(0)(inp)
	}
}
