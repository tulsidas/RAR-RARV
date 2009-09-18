import Params._

class LocalInsert(inst: Instance, solucion: List[List[Customer]], nonvisit: List[Customer]) {

	val cust = solucion.foldLeft(0)(_ + _.size - 1)
	
	var mejor = solucion
	
	def insert(): List[List[Customer]] = {
		// pruebo meter los no visitados
		nonvisit.foreach(insert)

		mejor
	}
	
	private def insert(nv: Customer): Unit = {
		mejor.foreach { camion =>
			for (pos <- 1 to camion.length) {
				val nuevo = camion.take(pos) ++ List(nv) ++ camion.drop(pos)
				if (inst.camionFactible(nuevo)) {
					mejor = nuevo :: (mejor - camion)
					return
				}
			}
		}
	}
}
