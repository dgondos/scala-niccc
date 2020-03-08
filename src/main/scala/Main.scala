import java.io.BufferedInputStream
import java.io.FileInputStream
import scala.util.control.Breaks._
import scala.collection.mutable
import scala.swing.Swing
import scala.swing.Frame
import scala.swing.FlowPanel
import scala.swing.Label
import scala.swing.Button
import scala.swing.SimpleSwingApplication
import scala.swing.MainFrame
import scala.swing.Panel
import java.awt.Color
import java.awt.Graphics2D
import java.awt.Point
import java.awt.geom.GeneralPath
import java.awt.Dimension
import scala.swing.event.MouseReleased
import scala.swing.event.MousePressed
import scala.concurrent.Future

object Hello extends SimpleSwingApplication {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    object ui extends Panel {
        background = Color.black
        preferredSize = new Dimension(255, 200)

        focusable = true
        listenTo(mouse.clicks, mouse.moves, keys)

        reactions += {
            case e: MouseReleased => Future(startDraw())
        }

        /* records the dragging */
        var path = new GeneralPath
        path.moveTo(0,0)

        def lineTo(p: Point): Unit = {
            path.lineTo(p.x, p.y); repaint()
        }

        def moveTo(p: Point): Unit = {
            path.moveTo(p.x, p.y); repaint()
        }

        def clear(): Unit = {
            path = new GeneralPath
            repaint()
        }

        override def paintComponent(g: Graphics2D): Unit = {
            super.paintComponent(g)
            g.setColor(Color.white)
            val h = size.height
            g.drawString("Press left mouse button to start.", 10, h - 26)
            g.setColor(Color.white)
            g.draw(path)
        }
    }

    def top: Frame = new MainFrame {
        title = "NICCC"
        contents = ui
    }

    val isBlank = (f: Int) =>  {
        (f & (1L << 0)) != 0
    }
    val isPalette = (f: Int) => {
        (f & (1L << 1)) != 0
    }
    val isIndexed = (f: Int) => {
        (f & (1L << 2)) != 0
    }
    def readByte (it: Iterator[Byte]) : Int = {
        it.next() & 0xFF // stupid java
    }
    def mkWord16eb (l: Int, r: Int) : Int = {
        ((l << 8) | r)
    }
    def readDescriptor(d: Int) : (Int, Int) = {
        ((d & 0xF0) >>> 4, d & 0xF)
    }
    def readPalette(it: Iterator[Byte], p: Array[Int]) : Unit = {
        val l = readByte(it)
        val r = readByte(it)
        val bitmask = mkWord16eb(l, r)
        for (i <- 0 to 15) {
            val set = (bitmask & (1L << i)) != 0
            if (set) {
                val cl = readByte(it)
                val cr = readByte(it)
                p.update(15 - i, mkWord16eb(cl, cr))
            }
        }
    }
    def readIndexedVerts(it: Iterator[Byte]) : List[(Int, Int)] = {
        var verts = List[(Int, Int)]()

        val nVerts = readByte(it)
        for (i <- 0 until nVerts) {
            val x = readByte(it)
            val y = readByte(it)
            verts = verts :+ (x, y)
        }
        verts
    }

    val bis = new BufferedInputStream(new FileInputStream("scene1.bin"))
    val bin: Array[Byte] = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray

    def startDraw(): Unit = {
        var frames = 0
        for (b_i <- 0 until 9) {
            val block = bin.slice(b_i * 64 * 1024, (b_i + 1) * 64 * 1024)
            val it = block.iterator
            var endOfBlock = false
            while (!endOfBlock) { // no idea how many frames per block
                val b_flag = readByte(it)
                var indexedVerts = Array[(Int, Int)]()
                var polys = List[List[(Int, Int, Int)]]() // x,y,color
                var palette = Array[Int]()
                for (i <- 0 until 16) {
                    palette = palette :+ 0
                }

                breakable {
                    if (isBlank(b_flag)) {
                        ui.clear()
                    }
                    if (isPalette(b_flag)) {
                        readPalette(it, palette)
                    }
                    if (isIndexed(b_flag)) {
                        indexedVerts = indexedVerts ++ readIndexedVerts(it)
                    }

                    while (true) { // no idea a priori until when we have frame data
                        val d = readByte(it)
                        d match {
                            case 0xFF => { // end of frame
                                polys.foreach(p => {
                                    ui.moveTo(new Point(p.head._1, p.head._2))
                                    p.foreach(v => {
                                        ui.lineTo(new Point(v._1, v._2))
                                    })
                                })
                                Thread.sleep(33)
                                frames = frames + 1
                                break
                            }
                            case 0xFE => { // end of frame and block
                                endOfBlock = true
                                frames = frames + 1
                                break
                            }
                            case 0xFD => { // end of everything
                                endOfBlock = true
                                println("end of everything")
                                break
                            }
                            case _ => { // data
                                val (colorIdx, nbrVerts) = readDescriptor(d)
                                val color = palette(colorIdx)
                                var p = List[(Int, Int, Int)]()
                                for (i <- 0 until nbrVerts) {
                                    if (isIndexed(b_flag)) {
                                        val vert = indexedVerts(readByte(it))
                                        p = p :+ (vert._1, vert._2, color)
                                    }
                                    else {
                                        p = p :+ (readByte(it), readByte(it), color)
                                    }
                                }
                                polys = polys :+ p
                            }
                        }
                    }
                }
            }
        }
        println(s"Nbr frames: ${frames}")
    }

}