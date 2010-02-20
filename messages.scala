case object Stop

case class Hello(id: String)

case class Start(inst: Instance, mejor: List[List[Customer]], delta: Double, rotura: Double, roturaV: Double, lsRAR: Boolean, lsRARV: Boolean)

case class Mejor(sol: List[List[Customer]], id: String)
