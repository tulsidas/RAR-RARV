import java.awt._
import java.io.File
import java.awt.image._
import javax.imageio.ImageIO

object Imaginario {
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
}
