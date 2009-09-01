import scala.actors.{Actor, OutputChannel}
import scala.actors.Actor._
import scala.actors.remote.RemoteActor
import scala.actors.remote.RemoteActor._

object ReinaMain {
	def main(args: Array[String]) {
		val reina = new Reina(9010, 'ACS)
		reina.start()

		/*
		val seriousActor2 = actor {
			for (i <- 1 to 5) {
				reina ! Hello(new Formica("google.com", 9010, 'ACS))
				Thread.sleep(1000)
			}
		}
		*/

		null
	}
}

class Reina(port: Int, name: Symbol) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	var hormigas: List[OutputChannel[Any]] = Nil
	val inst = Solomon.load("r101.txt")

	def act {
		alive(port)
		register(name, self)

		loop {
			receive {
				case Hello => { 
					hormigas = sender :: hormigas
					sender ! Start(inst)
				}
			}
		}
	}
}
