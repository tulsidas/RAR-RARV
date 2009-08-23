object Main {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		val solver = new NearestNeighbour(inst)
		val nn = solver.solve

		println("NN = " + nn.map(_.map(_.num)))
		inst.initTau(nn)

		val ant = new Ant(inst)

		//for (i <- 1 to 50) {
			val sol_ant = ant.solve
			println("Ant = " + sol_ant.map(_.map(_.num)))
		//}

		//val customers = inst.customers.length

		//println("sol = " + nn.map(_.map(_.num)))
		//println("length = " + nn.foldLeft(0.0)(_ + sumd(_)))
		//println("vehiculos = " + nn.length)

		// inicializo feromonas
		//val sol = nn.map(_.map(_.num))
		//println(sol)//.zip(sol.tail))

		//l.zip(l.tail ::: List(l.head))
	}
}
