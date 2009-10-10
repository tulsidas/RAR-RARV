import scala.util.Random
import Params._

class AntV(inst: Instance) extends Ant(inst) {
	var nonVisited = new scala.collection.mutable.HashMap[Customer, Int]()
	
	/**
	 * el valor heurístico de distancia entre nodo y v, tomando en cuenta nonVisited
	 */	
	override def η(nodo: Customer, v: Customer): Double = {
		val distancia = Math.max(1, inst.distancia(nodo, v) - nonVisited.getOrElse(v, 0)*ω)
		//inst.tau(nodo, v) * Math.pow((1.0 / distancia), β)
		inst.tau(nodo, v) / distancia
	}
}
