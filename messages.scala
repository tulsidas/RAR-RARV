case object Stop
case class Start(inst: Instance, mejorLargo: Double, mejorAtendidos: Int)

case class Hello(id: String)
case class MejorSolucion(sol: List[List[Customer]], id: String)
