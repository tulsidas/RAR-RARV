import scala.actors.{Actor, OutputChannel, TIMEOUT}
import scala.actors.Actor._
import scala.actors.remote.{RemoteActor, Node}
import scala.actors.remote.RemoteActor._
import scala.actors.Debug
import scala.collection.mutable.Map

object ReinaMain {
	def main(args: Array[String]) {
		if (args.length < 3) {
			println("Reina input hormigas/core minutos")
			exit
		}

		// arranco la reina
		val reina = new Reina(args(0), args(2).toInt, 9010, 'ACS)
		reina.start()

		// arranco hormigas en los nucleos
		val cores = Runtime.getRuntime().availableProcessors()

		for (i <- 1 to cores) {
			for (h <- 1 to args(1).toInt) {
				new Formica("localhost", 9010, 'ACS).start()
			}
		}
	}
}

class Reina(file: String, min: Int, port: Int, name: Symbol) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	val hormigas = Map.empty[String, OutputChannel[Any]]
	val inst = Solomon.load(file)
	val solver = new NearestNeighbour(inst)

	// nearest neighbour optimizado
	var mejor = new LocalSearch(inst, solver.solve).search()

	inst.globalTau(mejor)

	var mejorLargo = inst.solLength(mejor)
	var mejorVehiculos = mejor.length
	
	// actualizo el maximo de vehiculos permitidos a lo que me dio NN
	inst.vehiculos = mejorVehiculos
	
	println("NN: " + mejorLargo + " | " + mejorVehiculos)
	Debug.level = 1

	val queenActress = select(Node("localhost", 9010), 'ACS)

	// helper para cortar el main loop
	actor {
		Thread.sleep(min * 60 * 1000)
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
					println(id + " <-- Hello")
					sender ! Start(inst, mejorLargo, mejorVehiculos)
				}
				case MejorSolucion(newMejor, id) => {
					// chequeo que efectivamente sea mejor
					val newLargo = inst.solLength(newMejor)
					val newVehiculos = newMejor.length
					
					// veo si mejoro vehiculos o largo
					if (newVehiculos < mejorVehiculos 
						|| (newLargo < mejorLargo && newVehiculos <= mejorVehiculos)) {

						mejor = newMejor
						mejorLargo = newLargo
						mejorVehiculos = newVehiculos

						// sobreescribo feromonas, para mandar lo actualizado si se une una hormiga nueva
						inst overwriteTau(mejor)

						// actualizo al resto de las hormigas
						hormigas.filterKeys(uid => uid != id).foreach(p => p._2 ! MejorSolucion(mejor, ""))
						
						//println("mejor solucion = " + mejor.map(_.map(_.num)))
						println("Nuevo mejor: " + mejorLargo + " | " + mejorVehiculos)
					}
				}
				case TIMEOUT => {
					println("TIMEOUT")
					// fue, mando Stop al resto
					hormigas.foreach(p => p._2 ! Stop)

					running = false

					println("mejor solucion = " + mejor.map(_.map(_.num)))
					println("largo = " + inst.solLength(mejor))
					println("vehiculos = " + mejor.length)
				}
			}
		}
	}
}
