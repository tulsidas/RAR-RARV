import scala.actors.{Actor, Debug}
import scala.actors.Actor._
import scala.actors.remote._
import scala.actors.remote.RemoteActor._
import scala.util.Random

import java.util.UUID

import Params._

object RARMain {
	def main(args: Array[String]) {
		val host = args(0)
		val cores = Runtime.getRuntime().availableProcessors()

      for (i <- 1 to cores) {
         if (args.length > 1) {
            if (args(1).equals("v")) {
               new RAR(host, 9010, 'ACS, true).start()
            }
            else {
               new RAR(host, 9010, 'ACS, false).start()
            }
         }
         else {
            new RAR(host, 9010, 'ACS, i % 2 == 0).start() 
         }
      }
	}
}

class RAR(host: String, port: Int, name: Symbol, rarVehicular: Boolean) extends Actor {
	RemoteActor.classLoader = getClass().getClassLoader()

	val rnd = new Random()
	
	val id = UUID.randomUUID.toString
	
	var inst: Instance = null
	
	// la mejor solucion actual
	var mejor: List[List[Customer]] = Nil
	
	var πv: Double = 0
	var π: Double = 0
   var rotura:Double = 50
   var roturaV:Double = 50
   var lsRAR:Boolean = false
   var lsRARV:Boolean = false
	
	Debug.level = 1
	
	private[this] def updateProm(par: (Int, Float), nuevo: Float): (Int, Float) = {
		(par._1 + 1, (par._1 * par._2 + nuevo) / (par._1 + 1))
	}
	
	def act {
		val reina = select(Node(host, port), name)

		// hola		
		reina ! Hello(id)

		// espero Start
		receive {
			case Start(_inst, _mejor, _rotura, _roturaV, _lsRAR, _lsRARV) => {
				inst = _inst
				mejor = _mejor
				rotura = _rotura
				roturaV = _roturaV
				lsRAR = _lsRAR
				lsRARV = _lsRARV
				
				updatePi()
				
				if (rarVehicular) println(id + " RARV Start, πv: " + roturaV + ", LocalSearch: " + lsRARV)
				else println(id + " RAR Start, π: " + rotura + ", LocalSearch: " + lsRAR)
			}
		}

		var running = true
		var promLargo = (0, 0f)
		var promFactible = (0, 0f)
		var promVehiculos = (0, 0f)
		var intentos = 0
		
		val d = distancias
		val dp = d.reduceLeft(_+_) / d.size

		while(running) {
			if (mailboxSize > 0) {
				receive {
					case Stop => { 
                  println(if (rarVehicular) "RARV" else "RAR")
						println(id + " promedio rotura: " + promLargo._2)
						println(id + " promedio factibles: " + promFactible._2)
						println(id + " promedio vehiculos: " + promVehiculos._2)
						println(id + " intentos: " + intentos)
						//println("rotos: " + mapRoto.size + "\n" + mapRoto.map(p => (p._1.num, p._2)))
						running = false
					}
					case Mejor(newMejor, _) => {
						val newLargo = inst.solLength(newMejor)
						val newVehiculos = newMejor.length
					
						if (newVehiculos < mejor.length || 
							(newVehiculos == mejor.length && newLargo < inst.solLength(mejor))) {
				         //println(id + " recibo Mejor " + newLargo + " | " + newVehiculos)

                     if (newVehiculos < mejor.length) {
   							mejor = newMejor
   							updatePi()
							}
							else {
   							mejor = newMejor
							}
						}
					}
				}
			}
			else {
				val rotos = 
					if (rarVehicular) {
						if (rnd.nextDouble > δ) {
							// entre d/3 y 2d/3
							ruinDistV(dp/3 + rnd.nextDouble*(dp/3), mejor)
						}
						else {
   						ruinRndV(mejor) 
						}
					}
					else {
						if (rnd.nextDouble > δ) {
							// entre d/3 y 2d/3
							ruinDist(dp/3 + rnd.nextDouble*(dp/3), mejor)
						}
						else {
							ruinRnd(mejor)
						}
					}
					
//			   println("rotos = " + rotos.length)

				val ryr = recreate(mejor, rotos)
				val factible = inst.factible(ryr)
				val vehiculos = ryr.length

            intentos = intentos + 1
				promLargo = updateProm(promLargo, rotos.length)
				promFactible = updateProm(promFactible, if (factible) 1 else 0)
				promVehiculos = updateProm(promVehiculos, vehiculos)

				if (factible) {
               val optimizado = 
                  if ((rarVehicular && !lsRARV) || !lsRAR) { ryr }
                  else { new LocalSearch(inst, ryr).search() }

					if (optimizado.length < mejor.length || 
						inst.solLength(optimizado) < inst.solLength(mejor)) {
						
						// un nuevo mejor
						if (optimizado.length < mejor.length) {
                     mejor = optimizado
                     updatePi()
						}
						else {
                     mejor = optimizado
						}
						
						reina ! Mejor(mejor, id)
					}
				}
				else {
					// println("recreate("+rotos.length+") | NO factible | " + vehiculos)
				}
			}
		}
	}

	private def recreate(solucion: List[List[Customer]], rotos: List[Customer]): List[List[Customer]] = {
		val arruinado = solucion.map(_ -- rotos).filter(_.length > 1)

		new LocalInsert(inst, arruinado).insert(rotos)
	}
	
	/** arruino random */
	private def ruinRnd(sol: List[List[Customer]]): List[Customer] = {
	   val β = if (rarVehicular) πv else π

      sol.foldLeft(List[Customer]()) { (acc, it) =>
         // .tail para que no saque el deposito
         val veh = it.tail
         var rotos = veh.filter(c => rnd.nextDouble > β)
         
         if (rotos.length == veh.length ) {
            // saco de los rotos uno al azar cosa que el vehiculo no desaparezca
            
            rotos -= rotos(rnd.nextInt(rotos.length))
         }
         
         acc ::: rotos
      }
	}
	
	/** arruino n vehiculos al azar y después random del resto */
	private def ruinRndV(sol: List[List[Customer]]): List[Customer] = {
		val vehiculo:List[Customer] = sol(rnd.nextInt(sol.length))// - inst.source
	   
		vehiculo ++ ruinRnd(sol - vehiculo) - inst.source
	}
	
	private def ruinDist(dist: Double, sol: List[List[Customer]]): List[Customer] = {
      val c = inst.customers.tail(rnd.nextInt(inst.customers.length - 1))
      
      sol.foldLeft(List[Customer]()) { (acc, it) =>
         // .tail para que no saque el deposito
         val veh = it.tail
         var rotos = veh.filter(inst.distancia(_, c) < dist)
         
         if (rotos.length == veh.length ) {
            // saco de los rotos uno al azar cosa que el vehiculo no desaparezca
            rotos -= rotos(rnd.nextInt(rotos.length))
         }
         
         acc ::: rotos
      }
	}

	/* arruino por distancia espacial a un cliente dado */
	private def ruinDistV(dist: Double, sol: List[List[Customer]]): List[Customer] = {
		val vehiculo:List[Customer] = sol(rnd.nextInt(sol.length))// - inst.source
		
		vehiculo ++ ruinDist(dist, sol - vehiculo)
	}

	private def ruinK(cust: List[Customer], k:Int): List[Customer] = k match {
		case 0 => Nil
		case n => {
			val c = cust(rnd.nextInt(cust.size))
			c :: ruinK(cust - c, n-1)
		}
	}
	
	private def distancias: List[Double] = {
		def pares(clientes: List[Customer]): List[(Customer, Customer)] = {
			def zzip(cliente: Customer, resto: List[Customer]): List[(Customer, Customer)] = resto match {
				case Nil => Nil
				case xs => List(cliente).zipAll(xs, cliente, cliente)
			}

			clientes match {
				case Nil => Nil
				case x :: xs => zzip(x, xs) ::: pares(xs)
			}
		}
		
		pares(inst.customers).map( p => inst.distancia(p._1, p._2) )
	}
	
	private def updatePi() = {
      val clientes = mejor.foldLeft(0)(_ + _.size - 1).toFloat
      val vehiculos =  mejor.length.toFloat
      val ppv = clientes/vehiculos
      
      val ratioV = (clientes-ppv) / roturaV
      val ratio = clientes / rotura

      πv = 1 - 1 / ratioV
      π = 1 - 1 / ratio

//      println(id + "--> π = " + π + ", πv = " + πv)
	}
}	
