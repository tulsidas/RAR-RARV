import Params._

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
	
	var nonVisited = new scala.collection.mutable.HashMap[Customer, Int]()

	val id = UUID.randomUUID.toString
	Debug.level = 1

	val reina = select(Node(host, port), name)

	var inst: Instance = null
	
	val t = System.currentTimeMillis

	def act {
		var ant: AntV = null
		
		// hola		
		reina ! HelloV(id)

		// espero Start
		receive {
			case StartV(_inst, _mejor, _mejorCust) => {
				// 1 menos del mejor
				inst = _inst
				
				ant = new AntV(inst)
				
				mejor = _mejor
				mejorVehiculos = _mejor.length
				mejorCust = _mejorCust

				inst.vehiculos = mejorVehiculos - 1
				
				τ0 = 1 / (inst.customers.length * inst.solLength(mejor))
				
				println("Start! buscando con " + inst.vehiculos)
			}
		}

		var running = true

		while(running) {
			if (mailboxSize > 0) {
				receive {
					case Stop => { running = false }
					case MejorVehiculos(newMejor, newTau, _) => {
						// guardo el nuevo mejor, si es realmente mejor
						val vehiculos = newMejor.length
						
						if (vehiculos < mejorVehiculos+1) {
							mejor = newMejor
							mejorVehiculos = vehiculos
							mejorCust = 0

							// sobreescribo feromonas
							inst overwriteTau(newTau)
							
							// actualizo el max permitido
							inst.vehiculos = mejorVehiculos-1
							
							println((System.currentTimeMillis-t) + " | " + id + " || buscando " + inst.vehiculos + " vehiculos")
						}
					}
					case MejorCustomers(newMejor, newTau, _) => {
						// guardo el nuevo mejor, si es realmente mejor
						val vehiculos = newMejor.length
						val customers = newMejor.foldLeft(0)(_ + _.size - 1)
						
						if (vehiculos < mejorVehiculos && inst.factible(newMejor)) {
							mejor = newMejor
							mejorVehiculos = vehiculos
							mejorCust = 0

							// sobreescribo feromonas
							inst.overwriteTau(newTau)
							
							// actualizo el max permitido
							inst.vehiculos = mejorVehiculos-1
							
							println((System.currentTimeMillis-t) + " | " + id + " || buscando " + inst.vehiculos + " vehiculos")
						}
						else if (vehiculos == mejorVehiculos && customers > mejorCust) {
							mejorCust = customers
							mejor = newMejor
							
							println((System.currentTimeMillis-t) + " | " + id + " || buscando " + inst.vehiculos + " vehiculos y más de " + mejorCust + " clientes")

							// sobreescribo feromonas
							inst.overwriteTau(newTau)
						}
					}
				}
			}
			else {
				ant.nonVisited = nonVisited

				val sAnt = ant.solve

				chequearSolucion(sAnt)
				
				// inst.globalTau(mejor)
			}
		}
	}
	
	def chequearSolucion(sAnt: List[List[Customer]]) = {
		val customers = sAnt.foldLeft(0)(_ + _.size - 1)
		val vehiculos = sAnt.length
		
		if (inst.factible(sAnt)) {
			if (vehiculos < mejorVehiculos) {
				// reduci vehiculos fehacientemente

				mejorVehiculos = vehiculos
				mejor = sAnt

				// establezco el nuevo máximo de vehículos (menos 1)
				inst.vehiculos = mejorVehiculos-1
				mejorCust = 0

				println((System.currentTimeMillis-t) + " | " + id + " || encontre " + vehiculos + " vehiculos")

				reina ! MejorVehiculos(mejor, inst.tauMap, id)
				
				// global update feromonas
				inst.globalTau(mejor)
			}
		}
		// no factible, pero puedo mejorar con inserciones
		else {
			val visited = sAnt.foldLeft(List[Customer]())(_ ++ _)
			val faltantes = inst.customers -- visited
		
			// actualizo vector nonVisited
			faltantes.foreach(nv => nonVisited.put(nv, nonVisited.getOrElseUpdate(nv, 0)+1))

			// busqueda de inserciones
			val inserted = new LocalInsert(inst, sAnt, faltantes, nonVisited).insert()
			val custInserted = inserted.foldLeft(0)(_ + _.size - 1)
			
			// consegui insertar y obtener más visitas, veo qué onda
			if (custInserted > mejorCust) {
				// limpio mapa de no-visitados
				nonVisited.clear

				mejorVehiculos = inserted.length
				mejor = inserted
				mejorCust = custInserted

				if (inst.factible(inserted)) {
					// establezco el nuevo máximo de vehículos (menos 1)
					inst.vehiculos = mejorVehiculos-1
					mejorCust = 0

					println((System.currentTimeMillis-t) + " | " + id + " || encontre " + mejorVehiculos + " vehiculos")
					reina ! MejorVehiculos(mejor, inst.tauMap, id)
					
					// global update feromonas
					inst.globalTau(mejor)
				}
				else {
					println((System.currentTimeMillis-t) + " | " + id + " || encontre " + mejorVehiculos + " vehiculos y " + mejorCust + " clientes")
					reina ! MejorCustomers(mejor, inst.tauMap, id)
					
					// global update feromonas
					inst.globalTau(mejor)
				}
			}
		}
	}
}
