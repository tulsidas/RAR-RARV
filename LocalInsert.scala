import scala.collection.Map

class LocalInsert(inst: Instance, solucion: List[List[Customer]], 
	nonvisit: List[Customer], nvMap: Map[Customer, Int]) {

	val cust = solucion.foldLeft(0)(_ + _.size - 1)
	
	var mejor = solucion
	
	def insert(): List[List[Customer]] = {
		// pruebo meter los no visitados (el menos visitado primero)
		nonvisit.sort((c1, c2) => nvMap.getOrElse(c1, 0) > nvMap.getOrElse(c2, 0)).foreach(insert)

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
