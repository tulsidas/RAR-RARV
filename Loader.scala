import scala.io.Source
import scala.collection.mutable.Set

object Loader {
	def load(file: String, inst: Instance): List[List[Customer]] = {
		val lines = Source.fromFile(file).getLines.toList

		var customers = List[List[Customer]]()

		for (n <- (0 until lines.length)) {
		  val params = lines(n).split("\\s+").toList
		  
		  customers = params.map(p => inst.customers(p.toInt)) :: customers
		}

		customers		
	}
}
