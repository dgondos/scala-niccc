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
import scala.None

object NICCC extends SimpleSwingApplication {
    implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

    object ui extends Panel {
        background = Color.black
        preferredSize = new Dimension(255, 200)
        focusable = true
        listenTo(mouse.clicks, mouse.moves, keys)

        var fg = Color.white
        var polys = List[(List[Point], Color)]()

        reactions += {
            case e: MouseReleased => Future(try { startDraw } catch { case e: Throwable => e.printStackTrace() } )
        }

        def updatePolys(p: List[(List[Point], Color)]): Unit = {
            polys = p
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

    class FrameFlag(f: Int) {
        def isBlank =  {
            (f & (1L << 0)) != 0
        }
        def isPalette = {
            (f & (1L << 1)) != 0
        }
        def isIndexed = {
            (f & (1L << 2)) != 0
        }
    }

    class PolyDescriptor(f: Int) {
        def nbrVerts = {
            f & 0xF
        }
        def colorIdx = {
            (f & 0xF0) >>> 4
        }
        def value = f
    }

    def decodeAtariSTColor(st: Int): Color = {
        //Atari ST format: 00000RRR0GGG0BBB
        val blue = (st & 0x7) << 4
        val green = ((st & 0x70) >>> 4) << 4
        val red = ((st & 0x700) >>> 8) << 4
        new Color(red, green, blue)
    }

    def mkWord16eb (l: Int, r: Int) : Int = {
        ((l << 8) | r)
    }

    def startDraw : Unit = {
        var palette = Array.fill(16)(Color.black)
        object data {
            val bin = new BufferedInputStream(new FileInputStream("scene1.bin"))
            var bytesRead = 0
            def next = {
                bytesRead = bytesRead + 1
                bin.read & 0xFF
            }
        }
        while(true) {
            val flag = new FrameFlag(data.next)
            val bitmask = if (flag.isPalette) mkWord16eb(data.next, data.next) else 0
            palette = if (flag.isPalette) (for (i <- 0 to 15) yield i).map(i => if ((bitmask & (1L << 15 - i)) != 0) decodeAtariSTColor(mkWord16eb(data.next, data.next)) else palette(i)).toArray else palette
            val indexedVerts = if (flag.isIndexed) (for (i <- 0 until data.next) yield i).map(_ => new Point(data.next, data.next)).toArray else Array[Point]()
            var d = new PolyDescriptor(0)
            val frameData = Stream.continually(d = new PolyDescriptor(data.next)).map(_ => if (List(0xFF,0xFE,0xFD).contains(d.value)) None else Some((for (i <- 0 until d.nbrVerts) yield i).map(_ => if (flag.isIndexed) indexedVerts(data.next) else new Point(data.next, data.next)).toList, palette(d.colorIdx))).takeWhile(p => p.isDefined)
            ui.updatePolys(frameData.filter(e => e.isDefined).map(e => e.get).toList)
            ui.repaint
            Thread.sleep(33)
            if (d.value == 0xFE) {
                // end of block, skip to next 64k
                while(data.bytesRead % (64 * 1024) != 0) {
                    data.next
                }
            }
        }
    }

}