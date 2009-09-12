import Params._

object LocalSearch {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)

		def sumd(l: List[Customer]): Double = {
			l.zip(l.tail).foldLeft(0.0)((x, y) => x + inst.distancia(y._1, y._2))
		}

		val mejor = solver.solve
		val mejorLargo = mejor.foldLeft(0.0)(_ + sumd(_))
		
		println("NN = " + mejor.map(_.map(_.num)))
		println("NN length = " + mejorLargo)
		
		println("busqueda local!")
		dosOpt(mejor)
	}
	
	def dosOpt(sol: List[List[Customer]]): List[List[Customer]] = {
		// pruebo hacer relocate del primer camion
		val rnd = new scala.util.Random()

		val customer = sol(0)(1+rnd.nextInt(sol(0).length-1))
		println("relocate de " + customer.num)

		println("camion original = " + sol(0).map(_.num))
		val camion = sol(0) - customer
		println("camion = " + camion.map(_.num))
		println("")
		
		for(i <- 1 to camion.length) {
			val l = camion.take(i) ::: List(customer) ::: camion.drop(i)
			println(l.map(_.num))
			println(
		}

		Nil
	}
}
