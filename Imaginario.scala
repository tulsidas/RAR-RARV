import java.awt._
import java.io.File
import java.awt.image._
import javax.imageio.ImageIO

object Imaginario {
	def main(args: Array[String]) {
		val inst = Solomon.load("r101.txt")
		val nn = new NearestNeighbour(inst)
		
		inst.globalTau(nn.solve)
		
		writeImage("out.jpg", inst)
	}
	
	def writeImage(file: String, inst: Instance) = {
		val size = 808
		val step = size / inst.customers.length	

		val buff = new BufferedImage(size, size, BufferedImage.TYPE_INT_RGB)
		
		val g = buff.createGraphics		
		
		for(i <- inst.customers; j <- inst.customers) {
			val x = inst.customers.indexOf(i) * step
			val y = inst.customers.indexOf(j) * step
			
			g.setColor(new Color(inst.tau(i,j).toInt))
			
			g.fillRect(x, y, step, step)
		}
		
		ImageIO.write(buff, "jpeg", new File(file));
	}
}
