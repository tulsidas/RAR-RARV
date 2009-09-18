import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.actors.Debug
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
			case Start(_inst, _mejorLargo, _mejorVehiculos) => {
				inst = _inst
				ant = new Ant(inst)
				mejorLargo = _mejorLargo
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
						mejorLargo = inst.solLength(mejor)
						mejorVehiculos = mejor.length
						
						println(id + " recibo solucion: " + mejorLargo +" | " + mejorVehiculos)

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
					// busqueda local en la nueva solucion
					val optimizado = new LocalSearch(inst, sAnt).search()

					val sal = inst.solLength(optimizado)
					val vehiculos = optimizado.length
					val cust = optimizado.foldLeft(0)(_ + _.size - 1)

					if (sal < mejorLargo || vehiculos < mejorVehiculos) {
						mejorLargo = sal
						mejorVehiculos = vehiculos
						mejor = optimizado

						reina ! MejorSolucion(mejor, id)
						
						// sobreescribo feromonas
						// inst overwriteTau(mejor)
						
						// establezco el nuevo máximo de vehículos
						inst.vehiculos = mejorVehiculos
					}
				}
			}
		}
	}
}
