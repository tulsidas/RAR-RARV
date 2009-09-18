import scala.collection.mutable.Map
import Params._

case class Instance(var vehiculos: Int, val capacidad: Int, val customers: List[Customer]) {
	val source = customers(0)

	// cache de distancias
	private[this] val distancias = Map.empty[(Customer, Customer), Double]
	val tauMap = Map.empty[(Customer, Customer), Double]
	
	/**
	 * Si la solución visita a todos los clientes usando a lo sumo los vehiculos disponibles
	 */
	def factible(sol: List[List[Customer]]): Boolean = {
		sol.foldLeft(0)(_ + _.size - 1) == customers.length-1 && sol.length <= vehiculos
	}
	
	def camionLength(l: List[Customer]): Double = {
		l.zip(l.tail++List(l.head)).foldLeft(0.0)((x, y) => x + distancia(y._1, y._2))
	}

	def solLength(sol: List[List[Customer]]): Double = {		
		sol.foldLeft(0.0)(_ + camionLength(_))
	}
	
	def camionFactible(camion: List[Customer]): Boolean = {
		var hora:Double = 0
		var disponible = capacidad

		def vecinoFactible(actual: Customer, prox: Customer): Boolean = {
			val dist = distancia(actual, prox)
			val servicio = Math.max(prox.ready, hora + dist)
			
			// llego antes del fin de la ventana y me alcanza la carga?
			if (servicio <= prox.due && disponible >= prox.demand) {
				// actualizo tiempo y carga
				hora = servicio + prox.service
				disponible -= prox.demand
				true
			}
			else {
				false
			}
		}
		
		def recorridoFactible(clientes: List[Customer]): Boolean = {
			clientes match {
				case Nil => true
				case x :: Nil => true
				case x :: y :: xs => vecinoFactible(x, y) && recorridoFactible(y :: xs)
			}
		}

		recorridoFactible(camion)
	}

	def maxTau: Double = tauMap.values.toList.sort(_>_).head

	def tau(a: Customer, b: Customer): Double = {
		//println("tau("+a+","+b+")")
		tauMap.getOrElse((a, b), 0)
	}

	def updateTau(a: Customer, b: Customer, τ: Double) = {
		//println("updateTau("+a.num+","+b.num+") -> " + τ)
		val par = (a, b)
		tauMap + ((par, τ))
	}

	def updateTau(par: (Customer, Customer), τ: Double) = {
		//println("updateTau("+a.num+","+b.num+") -> " + τ)
		tauMap + ((par, τ))
	}

	def overwriteTau(solucion: List[List[Customer]]) = {
		// adios lo que tenia
		tauMap clear

		globalTau(solucion)
	}

	def globalTau(solucion: List[List[Customer]]) = {
		val Δτ = 1 / solLength(solucion)

		// τij = (1-p)τij + p*Δτ
		val pares = List.flatten(solucion.map(c => c.zip(c.tail ::: List(c.head))))
		pares.foreach(par => updateTau(par, 
			(1-p) * tau(par._1, par._2) + p * Δτ )
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
