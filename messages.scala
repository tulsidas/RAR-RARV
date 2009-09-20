case object Stop

case class Hello(id: String)
case class HelloV(id: String)

case class Start(inst: Instance, mejor: List[List[Customer]])

case class MejorLargo(sol: List[List[Customer]], id: String)
case class MejorVehiculos(sol: List[List[Customer]], id: String)
case class MejorCustomers(sol: List[List[Customer]], id: String)
