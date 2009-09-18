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
		
		//Imaginario.writeImage(nf.format(0)+".jpg", inst, mejor)
		//Imaginario.writeTauImage(nf.format(0)+".jpg", inst)
		
		inst.globalTau(mejor)
		
		// cantidad de hormigas
		val m = 1

		τ0 = m / mejorLargo

		val ant = new Ant(inst)

		Imaginario.writeTauCSV(nf.format(0)+".csv", inst)

		for (i <- 1 to 500) {
			val sa = ant.solve
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
			//Imaginario.writeTauImage(nf.format(i)+".jpg", inst)
//			if (i % 10 == 0) {
				//Imaginario.writeTauCSV(nf.format(i)+".csv", inst)
//			}
		}
		
		Imaginario.writeTauCSV(nf.format(0)+"final.csv", inst)

		println("Mejor solucion = " + mejor.map(_.map(_.num)))
		println("Largo: " + mejorLargo)
	}
}
