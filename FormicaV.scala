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
						
						println(id + "/V recibo solucion de " + mejorVehiculos)

						// sobreescribo feromonas
						inst overwriteTau(mejor)
						
						// establezco el nuevo máximo de vehículos
						inst.vehiculos = mejorVehiculos-1
						mejorCust = 0
					}
				}
			}
			else {
				val sAnt = ant.solve
				
				if (inst.factible(sAnt)) {
					println("reduci vehiculos: " + mejorVehiculos + " -> " + sAnt.length)
					mejorVehiculos = sAnt.length
					mejor = sAnt
	
					// establezco el nuevo máximo de vehículos (menos 1)
					inst.vehiculos = mejorVehiculos-1
					mejorCust = 0

					reina ! MejorSolucion(mejor, id)
				}
				else if (sAnt.length < mejorVehiculos) {
					// faltan clientes por visitar, pero puede ser mejor
					val visited = sAnt.foldLeft(List[Customer]())(_ ++ _)
					val nonvisit = inst.customers -- visited
						
					val inserted = new LocalInsert(inst, sAnt, nonvisit).insert()
					val custInserted = inserted.foldLeft(0)(_ + _.size - 1)
					
					if (custInserted > mejorCust) {
						if (inst.factible(inserted)) {
							// mejora vehiculos
							println("reduci vehiculos (c/insert): " + mejorVehiculos + " -> " + inserted.length)

							mejorVehiculos = inserted.length
							mejor = inserted
		
							// establezco el nuevo máximo de vehículos (menos 1)
							inst.vehiculos = mejorVehiculos-1
							mejorCust = 0

							reina ! MejorSolucion(mejor, id)
						}
						else {
							// mejora customers
							println("mejore customers: " + mejorCust + " -> " + custInserted)

							mejor = inserted
							mejorCust = custInserted
							
							// reina ! MejorSolucion(mejor, id)
						}
					}
				}
				else {
					println("basura: " + (sAnt.length) + " | visitas: " + (sAnt.foldLeft(0)(_ + _.size - 1)))
				}
			}
		}
	}
}
