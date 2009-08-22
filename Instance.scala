import scala.collection.mutable.Map

case class Instance(vehiculos: Int, capacidad: Int, customers: List[Customer]) {
	val source = customers(0)

	// cache de distancias
	private[this] val distancias = Map.empty[(Customer, Customer), Double]
	private[this] val tauMap = Map.empty[(Customer, Customer), Double]

	def tau(a: Customer, b: Customer): Double = {
		val par = _par(a, b)
		if (tauMap.contains(par)) {  
			tauMap(par)  
		}  
		else {  
			0
		}
	}

	def updateTau(a: Customer, b: Customer, ŧ: Double) = {
		val par = _par(a, b)
		tauMap + ((par, ŧ))
	}

	def initTau(sol: List[List[Customer]]) = {
		def flatten[A](l: List[List[A]]): List[A] = {
			l match {
				case Nil => Nil
				case x :: Nil => x
				case x :: xs => x ::: flatten(xs)
			}
		}

		def sumd(l: List[Customer]): Double = {
			l.zip(l.tail).foldLeft(0.0)((x, y) => x + distancia(y._1, y._2))
		}

		val pares = flatten(sol.map(c => c.zip(c.tail ::: List(c.head))))

		// cantidad de hormigas
		val m = 1

		// largo de la solucion inicial
		val Cnn = sol.foldLeft(0.0)(_ + sumd(_))

		//m / Cnn		
		val init = m / Cnn
		println("initTau = " + init)
		pares.foreach(par => updateTau(par._1, par._2, init))
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

	private def _par(a: Customer, b: Customer): (Customer, Customer) = {
		if (a.num < b.num) (a,b) else (b,a)
	}

	private def _distancia(c1: Customer, c2:Customer): Double = {
		val difx = c1.pos._1 - c2.pos._1
		val dify = c1.pos._2 - c2.pos._2
		Math.sqrt((difx*difx).toDouble + (dify*dify).toDouble)
	}
}
