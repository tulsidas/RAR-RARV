import scala.actors.{Actor, Debug}
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.util.Random

import java.util.UUID

import Params._

object RARMain {
	def main(args: Array[String]) {
		val host = args(0)
		val cores = Runtime.getRuntime().availableProcessors()

		for (i <- 1 to cores) {
			new RAR(host, 9010, 'ACS, i % 2 == 0).start()
		}
	}
}

class RAR(host: String, port: Int, name: Symbol, rarVehicular: Boolean) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	val rnd = new Random()
	
	val id = UUID.randomUUID.toString
	
	var inst: Instance = null
	
	// la mejor solucion actual
	var mejor: List[List[Customer]] = Nil

	Debug.level = 1
	
	private[this] def updateProm(par: (Int, Float), nuevo: Float): (Int, Float) = {
		(par._1 + 1, (par._1 * par._2 + nuevo) / (par._1 + 1))
	}
	
	//var mapRoto = new scala.collection.mutable.HashMap[Customer, Int]()
	
	def act {
		val reina = select(Node(host, port), name)

		// hola		
		reina ! Hello(id)

		// espero Start
		receive {
			case Start(_inst, _mejor) => {
				inst = _inst
				mejor = _mejor
				
				if (rarVehicular) println(id + " RARV Start")
				else println(id + " RAR Start")
			}
		}

		var running = true
		var promLargo = (0, 0f)
		var promFactible = (0, 0f)
		var promVehiculos = (0, 0f)
		
		val dp = distanciaPromedio
		
		//println("distanciaMax = " + distancias.sort(_>_).head)
		//println("distanciaMin = " + distancias.sort(_<_).head)
		//println("distanciaPromedio = " + dp)

		while(running) {
			if (mailboxSize > 0) {
				receive {
					case Stop => { 
						running = false
						println("promedio rotura: " + promLargo._2)
						println("promedio factibles: " + promFactible._2)
						println("promedio vehiculos: " + promVehiculos._2)
						//println("rotos: " + mapRoto.size + "\n" + mapRoto.map(p => (p._1.num, p._2)))
					}
					case Mejor(newMejor, _) => {
						val newLargo = inst.solLength(newMejor)
						val newVehiculos = newMejor.length
					
						if (newVehiculos < mejor.length || 
							(newVehiculos == mejor.length && newLargo < inst.solLength(mejor))) {
				         //println(id + " recibo Mejor " + newLargo + " | " + newVehiculos)

							mejor = newMejor
						}
					}
				}
			}
			else {
				rnd.setSeed(System.currentTimeMillis)
			
				val rotos = 
					if (rarVehicular) {
						ruinRndV(inst.customers.tail, 1) 
					}
					else {
						if (rnd.nextDouble > δ) {
							// entre d/3 y 2d/3
							ruinDist(dp/3 + rnd.nextDouble*(dp/3))
						}
						else {
							ruinRnd(inst.customers.tail)
						}

						// ruinTime(dist, inst.customers.tail(rnd.nextInt(inst.customers.length-1)))
					}
						
				val ryr = recreate(mejor, rotos)
				val factible = inst.factible(ryr)
				val vehiculos = ryr.length

				promLargo = updateProm(promLargo, rotos.length)
				promFactible = updateProm(promFactible, if (factible) 1 else 0)
				promVehiculos = updateProm(promVehiculos, vehiculos)

				if (factible) {
					//println("recreate("+rotos.length+") | factible | " + vehiculos + " | " + inst.solLength(ryr))
					val optimizado = new LocalSearch(inst, ryr).search()
					
					if (optimizado.length < mejor.length || 
						inst.solLength(optimizado) < inst.solLength(mejor)) {

						// un nuevo mejor
						mejor = optimizado
						
						reina ! Mejor(mejor, id)
					}
				}
				else {
					//println("recreate("+rotos.length+") | NO factible | " + vehiculos)
				}
			}
		}
	}

	private def recreate(solucion: List[List[Customer]], rotos: List[Customer]): List[List[Customer]] = {
		val arruinado = solucion.map(_ -- rotos).filter(_.length > 1)
		new LocalInsert(inst, arruinado).insert(rotos, false)
	}
	
	/** arruino random */
	private def ruinRnd(cust: List[Customer]): List[Customer] = {
		cust.filter(c => rnd.nextDouble > π)
	}
	
	/** arruino n vehiculos al azar y después random del resto */
	private def ruinRndV(customers: List[Customer], n: Int): List[Customer] = {
		val vehiculo:List[Customer] = mejor(rnd.nextInt(mejor.length)) - inst.source
		vehiculo ++ ruinRnd(customers -- vehiculo)
	}
	
	/** arruino por distancia espacial a un cliente dado */
	private def ruinDist(dist: Double): List[Customer] = {
		val customers = inst.customers.tail
		val c = customers(rnd.nextInt(customers.length))
		
		// println("ruin("+dist+","+c.num+")")

		customers.filter(inst.distancia(_, c) < dist)
	}

	/* arruino por distancia espacial a un cliente dado
	private def ruinDistV(dist: Double, c: Customer, n: Int, customers: List[Customer]): List[Customer] = {
		val vehiculo:List[Customer] = mejor(rnd.nextInt(mejor.length)) - inst.source
		vehiculo ++ ruinDist(dist, c, customers -- vehiculo)
	}
	*/

	/** arruino por distancia temporal a un cliente dado */
	private def ruinTime(dist: Double, cust: Customer): List[Customer] = {
		Nil
	}
	
	/*
	private def ruinW(cust: List[Customer], pesos: scala.collection.Map[Customer, Int]): List[Customer] = {
		val target = cust.size * π
		//println("\nruinW | " + cust.map(_.num) + " | " + cust.size + " | " + target)
		//println("pesos = " + pesos.map(p => (p._1.num, p._2)).toList)

		// rompo #clientes * π clientes obtenidos al azar segun peso ponderado

		var ret:List[Customer] = Nil
		
		// creo una lista replicando los pesos
		var remaining:List[Customer] = Nil
		
		cust.foreach { c =>
			val times = Math.max(1, pesos.getOrElse(c, 1))
			for(i <- 1 to times) {
				remaining = c :: remaining
			}
		}
		
		// println("remaining inicial = " + remaining.map(_.num))
		// println("remaining inicial = " + remaining.size)
		
		try {
			while (ret.size < target) {
				// saco uno de remaining
				val nuevo = remaining(rnd.nextInt(remaining.size))
			
				// saco todas las repeticiones de nuevo de remaining
				remaining = remaining.remove(_ == nuevo)
			
				ret = nuevo :: ret
			}
		}
		catch {
			case _ => println("EXCEPTION remaining = " + remaining.map(_.num))
		}

		//println("ret = " + ret.map(_.num))
		
		ret.toList
	}

	private def ruinK(cust: List[Customer], k:Int): List[Customer] = k match {
		case 0 => Nil
		case n => {
			val c = cust(rnd.nextInt(cust.size))
			c :: ruinK(cust - c, n-1)
		}
	}
	*/
	
	/** arruino por distancia al source */
	//private def ruin(dist: Int): List[Customer] = ruin(dist, inst.source)
		
	private def distanciaPromedio = {
		val d = distancias
		d.reduceLeft(_+_) / d.size
	}	
	
	private def distancias: List[Double] = {
		def pares(clientes: List[Customer]): List[(Customer, Customer)] = {
			def zzip(cliente: Customer, resto: List[Customer]): List[(Customer, Customer)] = resto match {
				case Nil => Nil
				case xs => List(cliente).zipAll(xs, cliente, cliente)
			}

			clientes match {
				case Nil => Nil
				case x :: xs => zzip(x, xs) ::: pares(xs)
			}
		}
		
		pares(inst.customers).map( p => inst.distancia(p._1, p._2) )
	}

	
	// TODO chequear arruines espaciales y temporales
}	
