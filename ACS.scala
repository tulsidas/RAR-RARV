import Params._

object ACS {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)

		def sumd(l: List[Customer]): Double = {
			l.zip(l.tail).foldLeft(0.0)((x, y) => x + inst.distancia(y._1, y._2))
		}

		//println("NN = " + nn.map(_.map(_.num)))
		var mejor = solver.solve
		var mejorLargo = mejor.foldLeft(0.0)(_ + sumd(_))
		println("NN length = " + mejorLargo)

		inst.globalTau(mejor)

		// cantidad de hormigas
		val m = 1

		τ0 = m / mejorLargo

		// val nf = new java.text.DecimalFormat("000")
		val ant = new Ant(inst)

		for (i <- 1 to 100) {
			val sa = ant.solve
			
			//println("Ant = " + sa.map(_.map(_.num)))
			//println("length = " + sa.foldLeft(0.0)(_ + sumd(_)))
			//println("vehiculos = " + sa.length)
			//println("factible = " + inst.factible(sa))

			val sal = sa.foldLeft(0.0)(_ + sumd(_))
			val sav = sa.length		

			if (sal < mejorLargo) {
				mejorLargo = sal
				mejor = sa
				println("mejor largo: " + mejorLargo)
			}

			// actualizacion de feromonas globales
			inst.globalTau(sa)
			
			// Imaginario.writeImage(nf.format(i)+".jpg", inst)
		}

		println("Mejor solucion = " + mejor.map(_.map(_.num)))
		println("Largo: " + mejorLargo)
	}
}
