import scala.util.Random
import scala.collection.immutable.TreeMap

class Ant(val inst: Instance) extends Solver {
	val rnd = new Random()

	// params
	val α = 1
	val β = 2 // .. 5
	val q0 = 0.9
	val p = 0.1

	def proximo(nodo: Customer, vecinos: List[Customer], 
					hora: Double, capacidad: Int): Customer = {
		/*implicit def orderedCustomer(c: Customer): Ordered[Customer] = new Ordered[Customer] {
			def compare(that: Customer): Int = {
			}
		}*/
		println("proximo de "+nodo.num)

		// filtro los que llego a tiempo y me alcanza la capacidad
		val factibles = vecinos.filter(vecino => factible(nodo, vecino, hora, capacidad))

		if (factibles.isEmpty) {
			null
		}
		else {
			// un mapa [Customer, Double] con (tau*η^β) precalculado
			val mapη = factibles.foldLeft(Map[Customer, Double]()) { 
				(m, v) => m(v) = inst.tau(nodo, v) * Math.pow((1.0/inst.distancia(nodo, v)), β)
			}

			val q = rnd.nextDouble

			if (q < q0) {
				// exploitation
				println("exploitation")
			
				println("factibles = " + factibles.map(n => n.num + "|" + mapη(n)))

				// obtengo el mayor η
				val bestη = mapη.values.toList.sort(_ > _).head
				//println("best = " + best.num + "(" + mapη(best) + ")")
				//best

				// si hay varios con el mismo η, obtengo uno al azar de esos
				val bests = factibles.filter(mapη(_) == bestη)
				println("bests = " + bests.map(_.num))
				if (bests.length > 1) {
					weightedCases(bests.map((_, bests.length)))
				}
				else {
					bests.head
				}
			}
			else {
				// exploration
				println("exploration")

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
			if (p < 0.0 || p > 1.0) error("invalid probability")

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
