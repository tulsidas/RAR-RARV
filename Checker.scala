import scala.io.Source
import scala.collection.mutable.Set

object Checker {
	def main(args: Array[String]) {
		val file = args(0)
		val lines = Source.fromFile(file).getLines.toList

		val inst = Solomon.load(lines(0).trim+".txt")
		
		// linea 1 = largo / vehiculos
		val line1 = lines(1).split("\\s+").toList
		val largo = java.lang.Double.parseDouble(line1(0))
		val vehiculos = Integer.parseInt(line1(1))

		var customers = List[List[Customer]]()

		for (n <- (2 until lines.length)) {
		  val params = lines(n).split("\\s+").toList
		  
		  customers = params.map(p => inst.customers(p.toInt)) :: customers
		}
		
		println("segun txt: " + largo + " | " + vehiculos)
		println("segun inst: " + inst.solLength(customers) + " | " + customers.length)
		println("factible: " + inst.factible(customers))
		println("visitados: " + customers.foldLeft(0)(_ + _.size - 1))
	}
}
