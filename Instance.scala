import scala.collection.mutable.Map

case class Instance(val vehiculos: Int, val capacidad: Int, val customers: List[Customer]) {
	val source = customers(0)

	// cache de distancias
	private[this] val distancias = Map.empty[(Customer, Customer), Double]
	private[this] val tauMap = Map.empty[(Customer, Customer), Double]

	def tau(a: Customer, b: Customer): Double = {
		val par = (a, b)
		if (tauMap.contains(par)) {
			tauMap(par)
		}  
		else {  
			0
		}
	}

	def updateTau(a: Customer, b: Customer, ŧ: Double) = {
		val par = (a, b)
		tauMap + ((par, ŧ))
	}

	def initTau(sol: List[List[Customer]]) = {
		def sumd(l: List[Customer]): Double = {
			l.zip(l.tail).foldLeft(0.0)((x, y) => x + distancia(y._1, y._2))
		}

		val pares = List.flatten(sol.map(c => c.zip(c.tail ::: List(c.head))))
		// (0 :: c).zip(c ::: List(0))

		// cantidad de hormigas
		val m = 1

		// largo de la solucion inicial
		val Cnn = sol.foldLeft(0.0)(_ + sumd(_))

		//m / Cnn		
		val init = m / Cnn
		pares.foreach(par => updateTau(par._1, par._2, init))

		println("tau = " + tauMap.map(p => "(" + p._1._1.num + "," + p._1._2.num + ")=" + p._2))
	}

	def distancia(a: Customer, b:Customer): Double = {
		val par = _par(a, b)
		if (distancias.contains(par)) {  
			distancias(par)  
		}  
		else {  
			val y = _distancia(par._1, par._2)
			distancias + ((par, y))
			y  
		}
	}

	def tiempo(actual: Customer, prox: Customer, hora: Double): Double = {
		val d = distancia(actual, prox)
		val tiempo = Math.max(0, prox.ready - (hora + d))

		tiempo + d
	}

	private def _par(a: Customer, b: Customer): (Customer, Customer) = {
		if (a.num < b.num) (a,b) else (b,a)
	}

	private def _distancia(c1: Customer, c2:Customer): Double = {
		val difx = c1.pos._1 - c2.pos._1
		val dify = c1.pos._2 - c2.pos._2
		Math.sqrt((difx*difx).toDouble + (dify*dify).toDouble)
	}
}
