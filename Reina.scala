import scala.actors.{Actor, OutputChannel}
import scala.actors.Actor._
import scala.actors.remote.RemoteActor
import scala.actors.remote.RemoteActor._
import scala.actors.Debug

object ReinaMain {
	def main(args: Array[String]) {
		// arranco la reina
		val reina = new Reina(9010, 'ACS)
		reina.start()

		// arranco hormigas en los nucleos restantes
		// val cores = Runtime.getRuntime().availableProcessors()

		/*for (i <- 1 to cores-1) {
			println("nueva hormiga local")
			new Formica("localhost", 9010, 'ACS).start()
		}*/
	}
}

class Reina(port: Int, name: Symbol) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	var hormigas: List[OutputChannel[Any]] = Nil
	val inst = Solomon.load("r101.txt")
	val solver = new NearestNeighbour(inst)
	var mejor = solver.solve
	inst.globalTau(mejor)

	def sumd(l: List[Customer]): Double = {
		l.zip(l.tail).foldLeft(0.0)((x, y) => x + inst.distancia(y._1, y._2))
	}

	var mejorLargo = mejor.foldLeft(0.0)(_ + sumd(_))
	println("NN length = " + mejorLargo)
	Debug.level = 9

	def act {
		alive(port)
		register(name, self)

		loop {
			receive {
				case Hello => { 
					hormigas = sender :: hormigas
					sender ! Start(inst, mejorLargo)
				}
				case MejorLargo(d) => {
					println("el mejor largo es " + d)
				}
			}
		}
	}
}
