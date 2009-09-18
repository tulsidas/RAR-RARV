import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.actors.Debug
import java.util.UUID

class FormicaV(host: String, port: Int, name: Symbol) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	// la mejor solucion actual
	var mejor: List[List[Customer]] = Nil
	
	// #de vehiculos de la mejor solucion
	var mejorVehiculos = Math.MAX_INT
	
	// #de clientes visitados de la mejor solucion
	var mejorCust = 0

	val id = UUID.randomUUID.toString
	Debug.level = 1

	def act {
		val reina = select(Node(host, port), name)
		var ant: Ant = null
		var inst: Instance = null
		
		def nuevoMejor(sol: List[List[Customer]], factible: Boolean) = {
			mejorVehiculos = sol.length
			mejor = sol
		
			val customers = sol.foldLeft(0)(_ + _.size - 1)
		
			if (factible) {
				// establezco el nuevo máximo de vehículos (menos 1)
				inst.vehiculos = mejorVehiculos-1
				mejorCust = 0
			}
			else {
				mejorCust = customers
			}

			reina ! MejorSolucion(mejor, id)
		
			// sobreescribo feromonas
			// inst overwriteTau(mejor)
		}


		// hola		
		reina ! HelloV(id)

		// espero Start
		receive {
			case StartV(_inst, _mejorVehiculos) => {
				// 1 menos del mejor
				inst = _inst
				inst.vehiculos = inst.vehiculos - 1
				
				ant = new Ant(inst)
				mejorVehiculos = _mejorVehiculos
			}
		}

		var running = true
		while(running) {
			if (mailboxSize > 0) {
				receive {
					case Stop => running = false
					case MejorSolucion(newMejor, _) => {
						// guardo el nuevo mejor
						mejor = newMejor
						mejorVehiculos = mejor.length
						mejorCust = 0
						
						println(id + "/V recibo solucion de " + mejorVehiculos)

						// sobreescribo feromonas
						inst overwriteTau(mejor)
						
						// establezco el nuevo máximo de vehículos
						inst.vehiculos = mejorVehiculos
					}
				}
			}
			else {
				val sAnt = ant.solve
				
				if (inst.factible(sAnt)) {
					if (sAnt.length < mejorVehiculos) {
						nuevoMejor(sAnt, true)
					}
				}
				else {
					// no factible, pero puede ser mejor
					val visited = sAnt.foldLeft(List[Customer]())(_ ++ _)
					val nonvisit = inst.customers -- visited
						
					val inserted = new LocalInsert(inst, sAnt, nonvisit).insert()
					val custInserted = inserted.foldLeft(0)(_ + _.size - 1)
						
					if (custInserted > mejorCust) {
						mejorCust = custInserted
						nuevoMejor(inserted, inst.factible(inserted))
					}
				}
			}
		}
	}
}
