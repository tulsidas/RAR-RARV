case object Stop

case class Hello(id: String)
case class Start(inst: Instance, mejorLargo: Double, mejorVehiculos: Int)

case class HelloV(id: String)
case class StartV(inst: Instance, mejorVehiculos: Int)

case class MejorSolucion(sol: List[List[Customer]], id: String)
