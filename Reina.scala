import scala.actors.Actor
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

	var hormigas: List[Formica] = Nil

	def act {
		alive(port)
		register(name, self)

		loop {
			receive {
				case Hello(f) => println("hola pianola! " + f)
				case str: String => println("string: " + str)
				case x: Int => println("int: " + x)
				case msg => println(msg + "|" + msg.asInstanceOf[AnyRef].getClass())
			}
		}
	}
}
