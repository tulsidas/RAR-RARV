import scala.actors.{Actor, OutputChannel, TIMEOUT}
import scala.actors.Actor._
import scala.actors.remote.{RemoteActor, Node}
import scala.actors.remote.RemoteActor._
import scala.actors.Debug
import scala.collection.mutable.Map
import Params._

object ReinaMain {
	def main(args: Array[String]) {
		if (args.length < 2) {
			println("Reina input minutos")
			exit
		}

		// arranco la reina
		val reina = new Reina(args(0), args(1).toInt, 9010, 'ACS)
		reina.start()

		// arranco hormigas en los nucleos
		val cores = Runtime.getRuntime().availableProcessors()

		var v = true
		for (i <- 1 to cores) {
			if (v) {
				new RAR("localhost", 9010, 'ACS).start()
			}
			else {
				new Formica("localhost", 9010, 'ACS).start()
			}
			v = !v
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
	//var mejor = Loader.load("sol_r103.txt", inst)
	
	var mejorLargo = inst.solLength(mejor)
	var mejorVehiculos = mejor.length

	inst.globalTau(mejor)
	τ0 = 1 / inst.customers.length * mejorLargo
	
	// actualizo el maximo de vehiculos permitidos a lo que me dio NN
	inst.vehiculos = mejorVehiculos
	
	println("NN = " + mejor.map(_.map(_.num)))
	println("NN: " + mejorLargo + " | " + mejorVehiculos)
	if (!inst.factible(mejor)) {
		println("NN no factible!!")
		println("visitados: " + mejor.foldLeft(0)(_ + _.size - 1))
		exit
	}
	
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
					// una hormiga comun
					hormigas + ((id, sender))
					sender ! Start(inst, mejor)
				}
				case Mejor(newMejor, id) => {
					// chequeo que efectivamente sea mejor
					val newLargo = inst.solLength(newMejor)
					val newVehiculos = newMejor.length
					
					//println("Mejor("+newLargo+" | "+newVehiculos+")")
					//println("actual = "+mejorLargo+" | "+mejorVehiculos)
					
					if (newVehiculos < mejorVehiculos || 
						(newVehiculos == mejorVehiculos && newLargo < mejorLargo)) {

						// actualizo a las hormigas largueras
						hormigas.filterKeys(uid => uid != id).foreach(p => p._2 ! Mejor(mejor, ""))
                  
                  println("REINA --> Mejor: " + newLargo + " | " + newVehiculos)
						
						mejor = newMejor
						mejorLargo = newLargo
						mejorVehiculos = newVehiculos

						// sobreescribo feromonas, para mandar lo actualizado si se une una hormiga nueva
						//inst overwriteTau(newTau)
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
