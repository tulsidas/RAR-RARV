import Params._

object ACS {
	def main(args: Array[String]) {
		val nf = new java.text.DecimalFormat("000")
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)

		var mejor = solver.solve
		var mejorLargo = inst.solLength(mejor)
		println("NN = " + mejor.map(_.map(_.num)))
		println("NN length = " + mejorLargo)
		
		Imaginario.writeImage(nf.format(0)+".jpg", inst, mejor)
		
		exit

		inst.globalTau(mejor)

		// cantidad de hormigas
		val m = 1

		τ0 = m / mejorLargo

		val ant = new Ant(inst)

		for (i <- 1 to 0) {
			val sa = ant.solve
			
			//println("Ant = " + sa.map(_.map(_.num)))
			//println("length = " + inst.solLength(sa))
			//println("vehiculos = " + sa.length)
			//println("factible = " + inst.factible(sa))

			val sal = inst.solLength(sa)
			val sav = sa.length		

			if (sal < mejorLargo) {
				mejorLargo = sal
				mejor = sa
				println("mejor largo: " + mejorLargo)
			}

			// actualizacion de feromonas globales
			inst.globalTau(sa)
			
			// Imaginario.writeImage(nf.format(i)+".jpg", inst)
			//Imaginario.writeImage(nf.format(i)+".jpg", inst, sa)
		}

		println("Mejor solucion = " + mejor.map(_.map(_.num)))
		println("Largo: " + mejorLargo)
	}
}
