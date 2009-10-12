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

		while(running) {
			if (mailboxSize > 0) {
				receive {
					case Stop => { 
						running = false
					}
					case Mejor(newMejor, _) => {
						val newLargo = inst.solLength(newMejor)
						val newVehiculos = newMejor.length
					
						if (newVehiculos < mejor.length || 
							(newVehiculos == mejor.length && newLargo < inst.solLength(mejor))) {
				         println(id + " recibo Mejor " + newLargo + " | " + newVehiculos)

							mejor = newMejor
						}
					}
				}
			}
			else {
				val ryr = recreate(mejor, ruin())
				if (inst.factible(ryr) && 
						(ryr.length < mejor.length || inst.solLength(ryr) < inst.solLength(mejor))) {

					//println(ryr.map(_.map(_.num)))

					// un nuevo mejor
					mejor = ryr
					
					println(id + " envio Mejor " + inst.solLength(ryr) + " | " + ryr.length)
					reina ! Mejor(mejor, id)
				}
			}
		}
	}

	
	/** arruino random */
	private def ruin(): List[Customer] = {
		inst.customers.tail.filter(c => rnd.nextDouble > π)
	}
	
	private def recreate(solucion: List[List[Customer]], rotos: List[Customer]): List[List[Customer]] = {
		val arruinado = solucion.map(_ -- rotos).filter(_.length > 1)
		new LocalInsert(inst, arruinado).insert(rotos, Map())
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
