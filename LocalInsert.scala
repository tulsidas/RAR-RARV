import scala.collection.Map

class LocalInsert(inst: Instance, solucion: List[List[Customer]], 
	nonvisit: List[Customer], nvMap: Map[Customer, Int]) {

	var mejor = solucion
	
	//var mejorOrig = solucion
	//var inagregables = List[Customer]()

	def insert(): List[List[Customer]] = {
		println("********insert()********")
		println(solucion.map(_.map(_.num)))
		println(mejor.length + " | " + mejor.foldLeft(0)(_ + _.size - 1) + " | " + inst.solLength(mejor))
		println("nonvisit: " + nonvisit.map(_.num))
		println("nvMap - " + nvMap.map(p => (p._1.num, p._2)))

		//val clientes = mejor.foldLeft(0)(_ + _.size - 1)

		// pruebo meter los no visitados (el menos visitado primero)
		nonvisit.sort((c1, c2) => nvMap.getOrElse(c1, 0) > nvMap.getOrElse(c2, 0)).foreach(insert)
		
		//val nclientes = mejor.foldLeft(0)(_ + _.size - 1)

		//println("mejore de " +clientes+ " a " +nclientes)
	
		/*
		println("inagregables = " + inagregables.map(_.num))		
		inagregables.foreach { c =>
			// pruebo agregarlo al ppio de todo
			val p = tryInsert(mejorOrig, c).size
			
			if (p > 0) {
				println(c.num + " agregado al principio tiene " + p + " opciones")
			}
		}
		*/

		println(mejor.length + " | " + mejor.foldLeft(0)(_ + _.size - 1) + " | " + inst.solLength(mejor))

		mejor
	}
	
	private def tryInsert(sol: List[List[Customer]], cust: Customer): List[List[List[Customer]]] = {
		var factibles = new scala.collection.mutable.ListBuffer[List[List[Customer]]]()

		mejor.foreach { camion =>
			for (pos <- 1 to camion.length) {
				val nuevo = camion.take(pos) ++ (cust :: camion.drop(pos))
				if (inst.camionFactible(nuevo)) {
					factibles += nuevo :: (mejor - camion)
				}
			}
		}
		
		factibles.toList
	}
	
	private def insert(nv: Customer): Unit = {
		val factibles = tryInsert(mejor, nv)
		
		println("hay " + factibles.size + " opciones para insertar el cliente [" + nv.num + "]")

		/*if (factibles.size == 0) {
			// agrego al customer a la lista de inagregables
			inagregables = nv :: inagregables
		}
		else*/ if (factibles.size > 0) {

			// me quedo con el de menor largo
			mejor = factibles.tail.foldLeft(factibles.head){ (a,b) => if (inst.solLength(a) < inst.solLength(b)) a else b }
		}
	}

	/*
	 * probar permutaciones hasta 6 (6! = 720)
	 *	
	def permute[T](list:List[T]):Set[List[T]] = list match {
		case Nil => Set()
		case t :: Nil => Set(list)
		case _ => Set[List[T]]() ++ (0 to list.size-1).map { n => 
			val t = list.splitAt(n)
			permute(t._1 ::: t._2.tail).map(tail => t._2.head :: tail)
		}.flatMap(x => x)
	}
	 */
}
