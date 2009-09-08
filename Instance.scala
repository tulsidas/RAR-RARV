import scala.collection.mutable.Map
import Params._

case class Instance(var vehiculos: Int, val capacidad: Int, val customers: List[Customer]) {
	val source = customers(0)

	// cache de distancias
	private[this] val distancias = Map.empty[(Customer, Customer), Double]
	private[this] val tauMap = Map.empty[(Customer, Customer), Double]
	
	def factible(sol: List[List[Customer]]): Boolean = {
		sol.foldLeft(0)(_ + _.size - 1) == customers.length-1 && sol.length <= vehiculos
	}

	def maxTau: Double = tauMap.values.toList.sort(_>_).head

	def tau(a: Customer, b: Customer): Double = {
		//println("tau("+a+","+b+")")
		val par = (a, b)
		if (tauMap.contains(par)) {
			tauMap(par)
		}  
		else {
			0
		}
	}

	def updateTau(a: Customer, b: Customer, τ: Double) = {
		//println("updateTau("+a.num+","+b.num+") -> " + τ)
		val par = (a, b)
		tauMap + ((par, τ))
	}

	def overwriteTau(solucion: List[List[Customer]]) = {
		// adios lo que tenia
		tauMap clear

		globalTau(solucion)
	}

	def globalTau(solucion: List[List[Customer]]) = {
		def sumd(l: List[Customer]): Double = {
			l.zip(l.tail).foldLeft(0.0)((x, y) => x + distancia(y._1, y._2))
		}

		val Δτ = solucion.foldLeft(0.0)(_ + sumd(_))

		// τij = (1-p)τij + Δτ
		val pares = List.flatten(solucion.map(c => c.zip(c.tail ::: List(c.head))))
		pares.foreach(par => updateTau(par._1, par._2, 
			(1-p) * tau(par._1, par._2) + p*Δτ )
		)
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
		val difx = c1.x - c2.x
		val dify = c1.y - c2.y
		Math.sqrt((difx*difx).toDouble + (dify*dify).toDouble)
	}
}
