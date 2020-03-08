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
import java.awt.Polygon

object Hello extends SimpleSwingApplication {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    object ui extends Panel {
        background = Color.black
        preferredSize = new Dimension(255, 200)
        var fg = Color.white
        var polys = List[(List[Point], Color)]()

        focusable = true
        listenTo(mouse.clicks, mouse.moves, keys)

        reactions += {
            case e: MouseReleased => Future(startDraw())
        }

        /* records the dragging */

        def drawPolys(p: List[(List[Point], Color)]): Unit = {
            polys = p
            repaint()
        }

        def clear(): Unit = {
            polys = List[(List[Point], Color)]()
            repaint()
        }

        override def paintComponent(g: Graphics2D): Unit = {
            super.paintComponent(g)
            g.setColor(Color.white)
            val h = size.height
            g.drawString("Press left mouse button to start.", 10, h - 26)
            polys.foreach(p => {
                val verts = p._1
                val color = p._2

                g.setColor(color)
                val path = new Polygon

                verts.foreach(v => {
                    path.addPoint(v.x, v.y)
                })
                g.fillPolygon(path)
            })
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
    def readPalette(it: Iterator[Byte], p: Array[Color]) : Array[Color] = {
        val l = readByte(it)
        val r = readByte(it)
        val bitmask = mkWord16eb(l, r)
        for (i <- 0 to 15) {
            val set = (bitmask & (1L << i)) != 0
            if (set) {
                val cl = readByte(it)
                val cr = readByte(it)
                p.update(15 - i, decodeAtariSTColor(mkWord16eb(cl, cr)))
            }
        }
        p
    }
    def decodeAtariSTColor(st: Int): Color = {
        //Atari ST format: 00000RRR0GGG0BBB
        val blue = (st & 0x7) << 4
        val green = ((st & 0x70) >>> 4) << 4
        val red = ((st & 0x700) >>> 8) << 4
        new Color(red, green, blue)
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
        var palette = Array[Color]()
        for (b_i <- 0 until 9) {
            val block = bin.slice(b_i * 64 * 1024, (b_i + 1) * 64 * 1024)
            val it = block.iterator
            var endOfBlock = false

            while (!endOfBlock) { // no idea how many frames per block
                val b_flag = readByte(it)
                var indexedVerts = Array[(Int, Int)]()
                var polys = List[(List[Point], Color)]()

                breakable {
                    if (isBlank(b_flag)) {
                        ui.clear()
                    }
                    if (isPalette(b_flag)) {
                        for (i <- 0 until 16) {
                            palette = palette :+ Color.black
                        }
                        palette = readPalette(it, palette)
                    }
                    if (isIndexed(b_flag)) {
                        indexedVerts = indexedVerts ++ readIndexedVerts(it)
                    }

                    while (true) { // no idea a priori until when we have frame data
                        val d = readByte(it)
                        d match {
                            case 0xFF => { // end of frame
                                ui.drawPolys(polys)
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
                                var verts = List[Point]()
                                for (i <- 0 until nbrVerts) {
                                    if (isIndexed(b_flag)) {
                                        val vert = indexedVerts(readByte(it))
                                        verts = verts :+ new Point(vert._1, vert._2)
                                    }
                                    else {
                                        verts = verts :+ new Point(readByte(it), readByte(it))
                                    }
                                }
                                polys = polys :+ (verts, color)
                            }
                        }
                    }
                }
            }
        }
        println(s"Nbr frames: ${frames}")
    }

}