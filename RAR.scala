import scala.actors.{Actor, Debug}
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.util.Random

import java.util.UUID

import Params._

class RAR(host: String, port: Int, name: Symbol) extends Actor {
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
	
	def act {
		val reina = select(Node(host, port), name)

		// hola		
		reina ! Hello(id)

		// espero Start
		receive {
			case Start(_inst, _mejor) => {
				inst = _inst
				mejor = _mejor
				
				println(id + " RAR Start!")
			}
		}

		var running = true
		var promLargo = (0, 0f)
		var promFactible = (0, 0f)
		var promVehiculos = (0, 0f)

		while(running) {
			if (mailboxSize > 0) {
				receive {
					case Stop => { 
						running = false
						println("promedio rotura: " + promLargo._2)
						println("promedio factibles: " + promFactible._2)
						println("promedio vehiculos: " + promVehiculos._2)
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
				val rotos = ruin()
				//val rotos = ruinV(1)
				val ryr = recreate(mejor, rotos)
				val factible = inst.factible(ryr)
				val vehiculos = ryr.length
				
				promLargo = updateProm(promLargo, rotos.length)
				promFactible = updateProm(promFactible, if (factible) 1 else 0)
				promVehiculos = updateProm(promVehiculos, vehiculos)

				//println("rotos = " + rotos.map(_.num))
				if (factible) {
					//print("recreate("+rotos.length+") | factible | " + vehiculos + " | " + inst.solLength(ryr))
					val optimizado = ryr // TODO new LocalSearch(inst, ryr).search()
					//println(" ==> " + optimizado.length + " | " + inst.solLength(optimizado))
					
					if (optimizado.length < mejor.length || 
						inst.solLength(optimizado) < inst.solLength(mejor)) {

						//println(ryr.map(_.map(_.num)))

						// un nuevo mejor
						mejor = optimizado
					
						//println(id + " envio Mejor " + inst.solLength(ryr) + " | " + ryr.length)
						reina ! Mejor(mejor, id)
					}

				}
				else {
					//println("recreate("+rotos.length+") | NO factible | " + vehiculos)
				}
			}
		}
	}

	
	/** arruino random */
	private def ruin(): List[Customer] = {
		inst.customers.tail.filter(c => rnd.nextDouble > π)
	}
	
	/** arruino n vehiculos al azar y después random del resto */
	private def ruinV(n: Int): List[Customer] = {
		val vehiculo:List[Customer] = mejor(rnd.nextInt(mejor.length)) - inst.source
		//println("vehiculo roto = " + vehiculo.map(_.num))
		
		vehiculo ++ (inst.customers.tail -- vehiculo).filter(c => rnd.nextDouble > π)
	}	
	
	private def recreate(solucion: List[List[Customer]], rotos: List[Customer]): List[List[Customer]] = {
		val arruinado = solucion.map(_ -- rotos).filter(_.length > 1)
		new LocalInsert(inst, arruinado).insert(rotos)
	}
	
	/** arruino por distancia al source */
	//private def ruin(dist: Int): List[Customer] = ruin(dist, inst.source)
		
	/** arruino por distancia a un customer dado */
	/*private def ruin(dist: Int, cust: Customer): List[Customer] = {
		println("ruin("+dist+","+cust.num+")")
	
		inst.customers.filter(
			c => inst.distancia(c, cust) < dist && inst.distancia(c, cust) > 0)
	}*/	
}	
