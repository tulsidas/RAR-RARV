import scala.io.Source
import scala.collection.mutable.Set

class NearestNeighbour(val inst: Instance) extends Solver {
	def proximo(nodo: Customer, vecinos: List[Customer], 
					hora: Double, capacidad: Int): Customer = {

		// filtro los que llego a tiempo y me alcanza la capacidad
		val factibles = vecinos.filter(vecino => factible(nodo, vecino, hora, capacidad)).sort(
			(v1, v2) => inst.tiempo(nodo, v1, hora) < inst.tiempo(nodo, v2, hora))

		// el mas cercano dentro de los factibles
		if (factibles.isEmpty) {
			null
		}
		else {
			factibles.head
		}
	}
}
