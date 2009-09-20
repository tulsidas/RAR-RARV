import Params._

import scala.actors.Actor
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.actors.Debug

import java.util.UUID

object FormicaVTest {
	def main(args: Array[String]) {
		val inst = Solomon.load(args(0))
		
		val nn = new LocalSearch(inst, new NearestNeighbour(inst).solve).search()
		τ0 = 1 / (inst.customers.length * inst.solLength(nn))
		inst.globalTau(nn)

		println("nn = " + nn.map(_.map(_.num)))

		var clientes = nn.foldLeft(0)(_ + _.size - 1)
		var vehiculos = nn.length

		// intento 1 menos
		inst.vehiculos = 4 // nn.length - 1		
		val antv = new AntV(inst)
		
		val nf = new java.text.DecimalFormat("0000")
		
		for (i <- 0 to 300) {
			val sol = antv.solve
			
			if (i % 1 == 0) {
				Imaginario.writeTauAndAnt(nf.format(i)+".jpg", inst, sol)
			}
			
			// println(sol.map(_.map(_.num)))
			
			// global update feromonas (sólo si es mejor)
			val nClientes = sol.foldLeft(0)(_ + _.size - 1)
			val nVehiculos = sol.length
			
			if (nVehiculos < vehiculos || (nVehiculos == vehiculos && nClientes > clientes)) {
				println("mejor solucion - " + nVehiculos + " | " + nClientes)
				// una mejor solucion
				clientes = nClientes
				vehiculos = nVehiculos
				inst.globalTau(sol)
			}
		}
	}
}
