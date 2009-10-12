import scala.collection.mutable.Map

case object Stop

case class Hello(id: String)

case class Start(inst: Instance, mejor: List[List[Customer]])

case class Mejor(sol: List[List[Customer]], id: String)
