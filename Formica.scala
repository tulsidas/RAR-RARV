import Params._

import scala.actors.{Actor, Debug}
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._

import scala.collection.Map

import java.util.UUID

object FormicaMain {
	def main(args: Array[String]) {
		val host = args(0)
		val cores = Runtime.getRuntime().availableProcessors()

		for (i <- 1 to cores) {
			for (h <- 1 to args(1).toInt) {
				new Formica(host, 9010, 'ACS).start()
			}
		}
	}
}

class Formica(host: String, port: Int, name: Symbol) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	var mejorLargo: Double = Math.MAX_DOUBLE
	var mejorVehiculos: Int = Math.MAX_INT
	var mejor: List[List[Customer]] = Nil
	val id = UUID.randomUUID.toString
	Debug.level = 1
	
	def act {
		val reina = select(Node(host, port), name)
		var ant: Ant = null
		var inst: Instance = null

		// hola		
		reina ! Hello(id)

		// espero Start
		receive {
			case Start(_inst, _mejor) => {
				inst = _inst
				ant = new Ant(inst)
				mejor = _mejor
				mejorLargo = _mejor.length
				mejorVehiculos = _mejor.foldLeft(0)(_ + _.size - 1)
				
				τ0 = 1 / (inst.customers.length * inst.solLength(mejor))
			}
		}

		def guardarMejor(newMejor: List[List[Customer]], newTau: Map[(Customer, Customer), Double]) = {
			// guardo el nuevo mejor
			mejor = newMejor
			mejorLargo = inst.solLength(mejor)
			mejorVehiculos = mejor.length

         println(id + " <-- MejorSolucion " + mejorLargo + " | " + mejorVehiculos)

			// sobreescribo feromonas
			//inst overwriteTau(mejor)
			//inst globalTau(mejor)
			inst.overwriteTau(newTau)
			
			// establezco el nuevo máximo de vehículos
			inst.vehiculos = mejorVehiculos
		}

		var running = true
		while(running) {
			if (mailboxSize > 0) {
				receive {
					case Stop => running = false
					case MejorLargo(newMejor, newTau, _) => guardarMejor(newMejor, newTau)
					case MejorVehiculos(newMejor, newTau, _) => guardarMejor(newMejor, newTau)
				}
			}
			else {
				val sAnt = ant.solve
				
				if (inst.factible(sAnt)) {
					// busqueda local en la nueva solucion
					val optimizado = new LocalSearch(inst, sAnt).search()

					val sal = inst.solLength(optimizado)
					val vehiculos = optimizado.length

					if (vehiculos < mejorVehiculos) {
						mejorLargo = sal
						mejorVehiculos = vehiculos
						mejor = optimizado

						reina ! MejorVehiculos(mejor, inst.tauMap, id)
						
						// establezco el nuevo máximo de vehículos
						inst.vehiculos = mejorVehiculos
						
						// global update feromonas
						// inst.globalTau(mejor)
					}
					else if (sal < mejorLargo) {
						mejorLargo = sal
						mejor = optimizado

						reina ! MejorLargo(mejor, inst.tauMap, id)
						
						// global update feromonas
						// inst.globalTau(mejor)
					}
				}
				
				// global update feromonas
				inst.globalTau(mejor)
			}
		}
	}
}
