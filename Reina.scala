import scala.actors.{Actor, OutputChannel, TIMEOUT}
import scala.actors.Actor._
import scala.actors.remote.{RemoteActor, Node}
import scala.actors.remote.RemoteActor._
import scala.actors.Debug
import scala.collection.mutable.Map
import Params._
import java.util.Properties
import java.io.FileInputStream

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
				new RAR("localhost", 9010, 'ACS, i%2!=0).start()
			}
			else {
				new Formica("localhost", 9010, 'ACS).start()
			}
//			v = !v
		}
	}
}

class Reina(file: String, min: Int, port: Int, name: Symbol) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	val hormigas = Map.empty[String, OutputChannel[Any]]

	val counter = Map.empty[String, Int]

   val properties = new Properties()
   try {
      properties.load(new FileInputStream("params.properties"));
   }

   val rotura:Double = java.lang.Double.parseDouble(properties.getProperty("rotura", "50"))
   val roturaV:Double = java.lang.Double.parseDouble(properties.getProperty("roturaV", "50"))
   val lsRARV:Boolean = java.lang.Boolean.parseBoolean(properties.getProperty("lsRARV", "false"))

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
	
	if (!inst.factible(mejor)) {
		println("NN no factible!!")
		println("visitados: " + mejor.foldLeft(0)(_ + _.size - 1))
		exit
	}
	
	println("NN: " + mejorLargo + " | " + mejorVehiculos + " | prom/vehiculo: " + 
		mejor.foldLeft(0)(_ + _.size - 1).toFloat / mejorVehiculos.toFloat)

//   println("δ = " + δ + ", π = " + π + ", πv = " + πv)

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
		
		val startTime = System.currentTimeMillis

		while(running) {
			receive {
				case Hello(id) => { 
					// una hormiga comun
					hormigas + ((id, sender))
					sender ! Start(inst, mejor, rotura, roturaV, lsRARV)
				}
				case Mejor(newMejor, id) => {
					// chequeo que efectivamente sea mejor
					val newLargo = inst.solLength(newMejor)
					val newVehiculos = newMejor.length
					
					if (newVehiculos < mejorVehiculos || 
						(newVehiculos == mejorVehiculos && newLargo < mejorLargo)) {

						mejor = newMejor
						mejorLargo = newLargo
						mejorVehiculos = newVehiculos

						// actualizo a las hormigas largueras
						hormigas.filterKeys(uid => uid != id).foreach(p => p._2 ! Mejor(mejor, ""))
						
						counter + ((id, counter.getOrElse(id, 0)+1))
                  
                  val t = System.currentTimeMillis - startTime
                  println("["+t+"] " + id + " --> Mejor: " + newLargo + " | " + newVehiculos)

						// sobreescribo feromonas, para mandar lo actualizado si se une una hormiga nueva
						//inst overwriteTau(newTau)
					}
				}
				case TIMEOUT => {
					println("TIMEOUT")

					running = false

					println("mejor solucion = " + mejor.map(_.map(_.num)))
					println("largo = " + inst.solLength(mejor))
					println("vehiculos = " + mejor.length)
					println(counter)

					// fue, mando Stop al resto
					hormigas.foreach(p => p._2 ! Stop)
				}
			}
		}
	}
}
