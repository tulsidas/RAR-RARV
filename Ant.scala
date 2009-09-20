import scala.util.Random
import scala.collection.immutable.TreeMap
import Params._

class Ant(val inst: Instance) extends Solver {
	val rnd = new Random()

	/**
	 * el valor heurístico de distancia entre nodo y v
	 */	
	def η(nodo: Customer, v: Customer): Double = {
		inst.tau(nodo, v) * Math.pow((1.0/inst.distancia(nodo, v)), β)
	}

	def proximo(nodo: Customer, vecinos: List[Customer], 
					hora: Double, capacidad: Int): Customer = {
		// filtro los que llego a tiempo y me alcanza la capacidad
		val insertables = vecinos.filter(vecino => insertable(nodo, vecino, hora, capacidad))

		if (insertables.isEmpty) {
			null
		}
		else {
			// un mapa [Customer, Double] con (tau*η^β) precalculado
			val mapη = insertables.foldLeft(Map[Customer, Double]()) { 
				(m, v) => m(v) = η(nodo, v)
			}

			val q = rnd.nextDouble

			if (q < q0) {
				// exploitation

				// obtengo el mayor η
				val values = mapη.values.toList
				val bestη = values.tail.foldLeft(values.head){ (a,b) => if (a > b) a else b }

				// si hay varios con el mismo η, obtengo el que cierra antes
				val bests = insertables.filter(mapη(_) == bestη)

				if (bests.length > 1) {
					bests.tail.foldLeft(bests.head) { (a,b) => if (a.due < b.due) a else b }
				}
				else {
					bests.head
				}
			}
			else {
				// exploration

				// el denominador
				val Σ = mapη.values.reduceLeft(_+_)

				// construyo una lista de [Customer, proba]
				val probas = mapη.map(t => (t._1, mapη(t._1) / Σ)) toList

				weightedCases(probas)
			}
		}
	}

	def weightedCases[A](inp: List[(A, Double)]): A = {
		def coinFlip[A](p: Double)(a1: A, a2: A) = {
			//if (p < 0.0 || p > 1.0) error("invalid probability: " + p)
			var prob = p
			if (p < 0) prob = 0
			if (p > 1) prob = 1
			
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
