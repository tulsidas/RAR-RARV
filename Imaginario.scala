import java.awt.{Color, Graphics2D, BasicStroke}
import java.io.File
import java.awt.image._
import javax.imageio.ImageIO

object Imaginario {
	def writeImage(file: String, inst: Instance, sol: List[List[Customer]]) = {
		val size = 800

		val scaleX = size / inst.customers.sort(_.x > _.x).head.x
		val scaleY = size / inst.customers.sort(_.y > _.y).head.y

		val buff = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
		
		val g = buff.createGraphics
		
		def drawCustomer(cust: Customer, color: Color) = {
			g.setColor(color)
			g.fillRect(cust.x * scaleX, cust.y * scaleY, 6, 6)
		}
		
		def drawLine(par: (Customer, Customer), color: Color) = {
			g.setColor(color)
			g.setStroke(new BasicStroke(2))
			g.drawLine(par._1.x * scaleX, par._1.y * scaleY, par._2.x * scaleX, par._2.y * scaleY)
		}

		// nodos
		drawCustomer(inst.source, Color.WHITE)
		inst.customers.drop(1).foreach(
			drawCustomer(_, Color.BLUE)
		)
		
		def colStream: Stream[Color] = {
			val colores = List(Color.CYAN, Color.DARK_GRAY, Color.GRAY, Color.GREEN,
									Color.LIGHT_GRAY, Color.MAGENTA, Color.ORANGE, Color.PINK,
									Color.WHITE, Color.YELLOW)
			colores.toStream append colStream
		}

		// soluciones
		sol.toStream.zip(colStream).foreach { stream =>
		//sol.filter(_.length > 5).toStream.zip(colStream).foreach { stream =>
			val l = stream _1
			val col = stream _2

			l.zip(l.tail ::: List(l.head)).foreach(
				drawLine(_, col)
			)
		}
		
		ImageIO.write(buff, "jpeg", new File(file));
	}
	
	/*
	def writeImage(file: String, inst: Instance) = {
		val size = 808
		val step = size / inst.customers.length
		val maxTau = inst.maxTau

		val buff = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
		
		val g = buff.createGraphics		
		
		for(i <- inst.customers.tail; j <- inst.customers.tail) {
			val x = inst.customers.indexOf(i) * step
			val y = inst.customers.indexOf(j) * step
			
			val color = (inst.tau(i,j) / maxTau).toFloat
			
			g.setColor(new Color(color, color, color))
			
			g.fillRect(x, y, step, step)
		}
		
		ImageIO.write(buff, "jpeg", new File(file));
	}
	*/
}
