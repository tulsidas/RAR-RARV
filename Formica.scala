import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._

object FormicaMain {
	def main(args: Array[String]) {
		val host = args(0)
		val cores = Runtime.getRuntime().availableProcessors()

		println(cores + " nucleos")

		for (i <- 1 to cores) {
			new Formica(host, 9010, 'ACS).start()
		}
	}
}

class Formica(host: String, port: Int, name: Symbol) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	def act {
		val reina = select(Node(host, port), name)
		var ant: Ant = null

		// hola		
		reina ! Hello

		var running = true
		while(running) {
			receive {
				case Start(inst) => {
					ant = new Ant(inst)
					println(ant.solve)
				}
			}
		}
	}
}
