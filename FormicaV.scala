import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.actors.Debug
import java.util.UUID

object FormicaVMain {
	def main(args: Array[String]) {
		val host = args(0)
		val cores = Runtime.getRuntime().availableProcessors()

		for (i <- 1 to cores) {
			for (h <- 1 to args(1).toInt) {
				new FormicaV(host, 9010, 'ACS).start()
			}
		}
	}
}

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
					case MejorVehiculos(newMejor, _) => {
						// guardo el nuevo mejor, si es realmente mejor
						val vehiculos = newMejor.length
						
						println(id + " recibo MejorVehiculos de #" + vehiculos)
						
						if (vehiculos < mejorVehiculos) {
							mejor = newMejor
							mejorVehiculos = vehiculos
							mejorCust = 0

							// sobreescribo feromonas
							inst overwriteTau(mejor)
							
							// actualizo el max permitido
							inst.vehiculos = mejorVehiculos-1
						}
					}
					case MejorCustomers(newMejor, _) => {
						// guardo el nuevo mejor, si es realmente mejor
						val vehiculos = newMejor.length
						val customers = newMejor.foldLeft(0)(_ + _.size - 1)
						
						println(id + " recibo MejorCustomers " + vehiculos + "|" + customers)
						
						if (vehiculos <= mejorVehiculos && customers > mejorCust) {
							mejor = newMejor
							mejorVehiculos = vehiculos
							mejorCust = customers

							// sobreescribo feromonas
							inst overwriteTau(mejor)
							
							// actualizo el max permitido
							inst.vehiculos = mejorVehiculos-1
						}
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

					reina ! MejorVehiculos(mejor, id)
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

							reina ! MejorVehiculos(mejor, id)
						}
						else {
							// mejora customers
							println("mejore customers: " + mejorCust + " -> " + custInserted)

							mejor = inserted
							mejorCust = custInserted
							
							reina ! MejorCustomers(mejor, id)
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
