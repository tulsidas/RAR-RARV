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
			new Formica(host, 9010, 'ACS).start()
		}
	}
}

class Formica(host: String, port: Int, name: Symbol) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	var mejorLargo: Double = Math.MAX_DOUBLE
	var mejorVehiculos: Double = Math.MAX_DOUBLE
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
				println(System.currentTimeMillis + "\t|Start! ("+mejorLargo+" | " + mejorVehiculos + ")")
			}
		}

		def sumd(l: List[Customer]): Double = {
			l.zip(l.tail).foldLeft(0.0)((x, y) => x + inst.distancia(y._1, y._2))
		}

		var running = true
		while(running) {
			if (mailboxSize > 0) {
				receive {
					case Stop => running = false
					case MejorSolucion(newMejor, _) => {
						// guardo el nuevo mejor
						mejor = newMejor
						mejorLargo = mejor.foldLeft(0.0)(_ + sumd(_))
						mejorVehiculos = mejor.length

						println(System.currentTimeMillis + "\t|recibo mejor solucion: " + mejorLargo + " | " + mejorVehiculos)

						// sobreescribo feromonas
						inst overwriteTau(mejor)
					}
				}
			}
			else {
				val sa = ant.solve
				val sal = sa.foldLeft(0.0)(_ + sumd(_))
				val vehiculos = sa.length
				val cust = sa.foldLeft(0)(_ + _.size - 1)
				
				//println("largo = " + sal)
				//println("vehiculos = " + vehiculos)

				if (inst.factible(sa)) {
						if (sal < mejorLargo) {
							mejorLargo = sal
							mejor = sa
							println("encontre mejor largo: " + mejorLargo)
							reina ! MejorSolucion(mejor, id)
							
							// actualizacion de feromonas globales
							inst.globalTau(sa)
						}
				}
				else {
					println("sol no factible")
				}
				//println("")
				/*
				if (vehiculos < mejorVehiculos) {
					mejorVehiculos = vehiculos
					mejor = sa
					println(System.currentTimeMillis + "\t|encontre mejor #vehiculos: " + mejorVehiculos)
					reina ! MejorSolucion(mejor, id)
				}
				*/
			}
		}
	}
}
