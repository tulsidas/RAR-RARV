import Params._

trait Solver {
	val inst: Instance
	val source = inst.source

	def solve: List[List[Customer]] = {
		val source = inst.source
		var disponibles = inst.customers - source
		var nn = List[List[Customer]]()

		while (!disponibles.isEmpty /*&& nn.length < inst.vehiculos*/) {
			// mando un camion
			val visitas = camion(disponibles)
			nn = nn ::: List(visitas)

			// saco los visitados por ese camion
			disponibles = disponibles -- visitas
		}

		nn
	}

	private def camion(nodos: List[Customer]): List[Customer] = {
		var hora:Double = 0
		var vecinos = nodos
		var actual = inst.source
		var capacidad = inst.capacidad
		var nn = actual :: Nil // List[Customer]()

		var prox:Customer = proximo(actual, vecinos, hora, capacidad)

		while (prox != null && !vecinos.isEmpty) {
			nn = nn ::: List(prox)

			// si llego antes que abra, a esperar				
			val tiempoEspera = Math.max(0, prox.ready - (hora + inst.distancia(actual, prox)))
			hora += tiempoEspera + prox.service + inst.distancia(actual, prox)
			capacidad -= prox.demand
			vecinos = vecinos - prox
			actual = prox

			prox = proximo(actual, vecinos, hora, capacidad)

			// actualizar tau local
			if (prox != null) {
				val τ = (1-ξ)*inst.tau(actual, prox) + ξ * τ0
				//println("τ = " + inst.tau(actual, prox) + " -> " + τ)
				inst.updateTau(actual, prox, τ)
			}
		}

		nn
	}

	def factible(nodo: Customer, vecino: Customer, hora: Double, capacidad: Int): Boolean = {
		val d = inst.distancia(nodo, vecino)
		hora + d < vecino.due && vecino.demand <= capacidad
	}

	def proximo(nodo: Customer, vecinos:List[Customer], hora: Double, capacidad: Int): Customer
}
