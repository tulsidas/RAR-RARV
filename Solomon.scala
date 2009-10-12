import scala.io.Source
import scala.collection.mutable.Set

object Solomon {
	def load(file: String): Instance = {
		val lines = Source.fromFile(file).getLines.toList

		// linea 4 = vehiculos / capacity
		val line4 = lines(4).split("\\s+").toList
		val vehiculos = Integer.parseInt(line4(1))
		val capacity = Integer.parseInt(line4(2))

		var customers = Set[Customer]()

		for (n <- (9 until lines.length)) {
		  val params = lines(n).split("\\s+").toList

		  val num = Integer.parseInt(params(1))
		  val x = Integer.parseInt(params(2))
		  val y = Integer.parseInt(params(3))
		  val demand = Integer.parseInt(params(4))
		  val ready = Integer.parseInt(params(5))
		  val due = Integer.parseInt(params(6))
		  val service = Integer.parseInt(params(7))

		  customers += new Customer(num, (x, y), demand, ready, due, service)
		}

		new Instance(vehiculos, capacity, 
			customers.toList.sort((c1,c2) => c1.num < c2.num))
	}
}
