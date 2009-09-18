case object Stop

case class Hello(id: String)
case class Start(inst: Instance, mejorLargo: Double, mejorVehiculos: Int)

case class HelloV(id: String)
case class StartV(inst: Instance, mejorVehiculos: Int)

case class MejorLargo(sol: List[List[Customer]], id: String)
case class MejorVehiculos(sol: List[List[Customer]], id: String)
case class MejorCustomers(sol: List[List[Customer]], id: String)
