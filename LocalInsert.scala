import scala.collection.Map

class LocalInsert(inst: Instance, solucion: List[List[Customer]]) {

	var mejor = solucion
	val ininsertables = new scala.collection.mutable.ListBuffer[Customer]()
	var nnv = List[Customer]()
	
	def insert(nonvisit: List[Customer]): List[List[Customer]] = {
		// println("\n********insert()********")
		// println(solucion.map(_.map(_.num)))
		// println(mejor.length + " | " + mejor.foldLeft(0)(_ + _.size - 1) + " | " + inst.solLength(mejor))
		// println("nonvisit: " + nonvisit.map(_.num))

		// pruebo meter los no visitados (el menos visitado primero)
		nonvisit.foreach(insert)

		if (ininsertables.size == 0) {
			mejor
		}
		else {
			nnv = nonvisit
			var i = 0
			while (ininsertables.size > 0 && i < 10) {
				i = i + 1

				nnv = ininsertables.toList ++ (nnv -- ininsertables.toList)
				//println("retry " + i)
				//println("ininsertables = " + ininsertables.map(_.num)) 
				//println("nueva nonvisit = " + nnv.map(_.num))

				ininsertables.clear
				mejor = solucion		// intento con la solucion inicial

				nnv.foreach(insert)
			}
			
			mejor // mal, guardar el mejor no el último
		}
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
	
	private def insert(nv: Customer) = {
		val factibles = tryInsert(mejor, nv)
		
		// TODO si el primero es ininsertable al pedo seguir intentando
		// println("hay " + factibles.size + " opciones para insertar el cliente [" + nv.num + "]")

		// me quedo con el de menor largo
		if (factibles.size > 0) {
			mejor = factibles.tail.foldLeft(factibles.head){ (a,b) => if (inst.solLength(a) < inst.solLength(b)) a else b }
		}
		else if (factibles.size == 0) {
			ininsertables + nv
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
