class NearestNeighbour(val inst: Instance) extends Solver {
	val rnd = new scala.util.Random()

	def proximo(nodo: Customer, vecinos: List[Customer], 
					hora: Double, capacidad: Int): Customer = {

		// filtro los que llego a tiempo y me alcanza la capacidad
		val insertables = vecinos.filter(vecino => insertable(nodo, vecino, hora, capacidad)).sort(
			(v1, v2) => inst.tiempo(nodo, v1, hora) < inst.tiempo(nodo, v2, hora))

		// el mas cercano dentro de los factibles
		if (insertables.isEmpty) {
			null
		}
		else {
			insertables.head
		}
	}
}
