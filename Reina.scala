import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote.RemoteActor
import scala.actors.remote.RemoteActor._

object ReinaMain {
	def main(args: Array[String]) {
		val reina = new Reina(9010, 'ACS)
		reina.start()
	}
}

class Reina(port: Int, name: Symbol) extends Actor {
	def act {
		alive(port)
		register(name, self)

		loop {
			receive {
				case msg => println("!" + msg)
			}
		}
	}
}
