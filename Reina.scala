import scala.actors.{Actor, OutputChannel, TIMEOUT}
import scala.actors.Actor._
import scala.actors.remote.{RemoteActor, Node}
import scala.actors.remote.RemoteActor._
import scala.actors.Debug
import scala.collection.mutable.Map

object ReinaMain {
	def main(args: Array[String]) {
		// arranco la reina
		val reina = new Reina(9010, 'ACS)
		reina.start()

		// arranco hormigas en los nucleos
		val cores = Runtime.getRuntime().availableProcessors()

		for (i <- 1 to cores) {
			println("nueva hormiga local")
			new Formica("localhost", 9010, 'ACS).start()
		}
	}
}

class Reina(port: Int, name: Symbol) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	val hormigas = Map.empty[String, OutputChannel[Any]]
	val inst = Solomon.load("r101.txt")
	val solver = new NearestNeighbour(inst)
	var mejor = solver.solve
	inst.globalTau(mejor)

	def sumd(l: List[Customer]): Double = {
		l.zip(l.tail).foldLeft(0.0)((x, y) => x + inst.distancia(y._1, y._2))
	}

	var mejorLargo = mejor.foldLeft(0.0)(_ + sumd(_))
	var mejorVehiculos = mejor.length
	
	// actualizo el maximo de vehiculos permitidos a lo que me dio NN
	inst.vehiculos = mejorVehiculos
	
	println("NN length = " + mejorLargo)
	println("NN vehiculos = " + mejorVehiculos)
	Debug.level = 1

	val queenActress = select(Node("localhost", 9010), 'ACS)

	// helper para cortar el main loop
	actor {
		Thread.sleep(2 * 60 * 1000)
		queenActress ! TIMEOUT
	}

	def act {
		alive(port)
		register(name, self)

		var running = true

		while(running) {
			receive {
				case Hello(id) => { 
					hormigas + ((id, sender))
					sender ! Start(inst, mejorLargo, mejorVehiculos)
				}
				case MejorSolucion(newMejor, id) => {
					// chequeo que efectivamente sea mejor
					val newLargo = newMejor.foldLeft(0.0)(_ + sumd(_))
					if (newLargo < mejorLargo) {
						mejor = newMejor
						mejorLargo = newLargo

					// chequeo que efectivamente sea mejor
					//val newV = newMejor.length
					//if (newV < mejorVehiculos) {
					//	mejor = newMejor
					//	mejorVehiculos = newV
					
						// sobreescribo feromonas, para mandar lo actualizado si se une una hormiga nueva
						inst overwriteTau(mejor)

						// actualizo al resto de las hormigas
						hormigas.filterKeys(uid => uid != id).foreach(p => p._2 ! MejorSolucion(mejor, ""))
						
						//println("mejor solucion = " + mejor.map(_.map(_.num)))
						println("mejor largo = " + mejor.foldLeft(0.0)(_ + sumd(_)))
						println("mejor vehiculos = " + mejor.length)
					}
				}
				case TIMEOUT => {
					println("TIMEOUT")
					// fue, mando Stop al resto
					hormigas.foreach(p => p._2 ! Stop)

					running = false

					println("mejor solucion = " + mejor.map(_.map(_.num)))
					println("largo = " + mejor.foldLeft(0.0)(_ + sumd(_)))
					println("vehiculos = " + mejor.length)
				}
			}
		}
	}
}
