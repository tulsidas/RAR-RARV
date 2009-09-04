import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.actors.Debug


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
	var mejor: List[List[Customer]] = Nil
	Debug.level = 9

	def act {
		val reina = select(Node(host, port), name)
		var ant: Ant = null
		var inst: Instance = null

		// hola		
		reina ! Hello

		// espero Start
		receive {
			case Start(_inst, _mejorLargo) => {
				inst = _inst
				println(this + " Start")
				ant = new Ant(inst)
				mejorLargo = _mejorLargo
			}
		}

		def sumd(l: List[Customer]): Double = {
			l.zip(l.tail).foldLeft(0.0)((x, y) => x + inst.distancia(y._1, y._2))
		}

		var running = true
		var i = 0
		while(running) {
			if (mailboxSize > 0) {
				receive {
					case Stop => running = false
				}
			}
			else {
				val sa = ant.solve
				//println("Ant = " + sa.map(_.map(_.num)))
				//println("length = " + sa.foldLeft(0.0)(_ + sumd(_)))
				//println("vehiculos = " + sa.length)
				val sal = sa.foldLeft(0.0)(_ + sumd(_))
				val sav = sa.length		

				if (sal < mejorLargo) {
					mejorLargo = sal
					mejor = sa
					println("mejor largo: " + mejorLargo)
					reina ! MejorLargo(mejorLargo)
				}

				if (i % 1000 == 0) {
					println("*")
					i = i + 1
				}

				// actualizacion de feromonas globales
				inst.globalTau(sa)
			}
		}
	}
}
