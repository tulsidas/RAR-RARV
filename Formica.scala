import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._

object FormicaMain {
	def main(args: Array[String]) {
		val formica = new Formica("localhost", 9010, 'ACS)
		formica.start()
	}
}

class Formica(host: String, port: Int, name: Symbol) extends Actor {
	def act {
		val reina = select(Node(host, port), name)

		// hola		
		reina ! this
	}
}
