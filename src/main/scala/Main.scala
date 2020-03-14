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
        preferredSize = new Dimension(250, 200)
        focusable = true
        listenTo(mouse.clicks, mouse.moves, keys)

        var fg = Color.white
        var polys = List[(Polygon, Color)]()

        reactions += {
            case e: MouseReleased => Future(startDraw)
        }

        def drawFrame(polys: List[(Polygon, Color)]) : Unit = {
            this.polys = polys
            repaint
        }

        override def paintComponent(g: Graphics2D): Unit = {
            super.paintComponent(g)
            g.setColor(Color.white)
            val h = size.height
            g.drawString("Press left mouse button to start.", 10, h - 26)
            polys.foreach(p => {
                g.setColor(p._2)
                g.fillPolygon(p._1)
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
                bin.read & 0xFF // stupid java
            }
        }

        val decodeNextColor = () => decodeAtariSTColor(mkWord16eb(data.next, data.next))
        val revBitSet = (i: Int, b: Int) => (b & (1L << 15 - i)) != 0
        val paletteEntry = (i: Int, b: Int) => if (revBitSet(i, b)) decodeNextColor() else palette(i)
        val mutatePalette = (b: Int) => (0 to 15).map(i => paletteEntry(i, b)).toArray
        val nextPoint = () => new Point(data.next, data.next)
        val nextVerts = (l: Int) => (0 until l).map(_ => nextPoint()).toArray

        while(data.bin.available != 0) {
            val frame = new FrameFlag(data.next)
            val bitmask = if (frame.isPalette) mkWord16eb(data.next, data.next) else 0
            palette = if (frame.isPalette) mutatePalette(bitmask) else palette
            val indexedVerts = if (frame.isIndexed) nextVerts(data.next) else Array[Point]()
            
            var d = new PolyDescriptor(0)

            val nextPointCached = (f: FrameFlag) => if (f.isIndexed) indexedVerts(data.next) else nextPoint()
            val nextPoly = () => ((0 until d.nbrVerts).map(_ => nextPointCached(frame)).toList, palette(d.colorIdx))
            val createPoly = (l: List[Point]) => new Polygon(l.map(v => v.x).toArray, l.map(v => v.y).toArray, l.length)
            val createColoredPoly = (p: (List[Point], Color)) => (createPoly(p._1), p._2)

            val polys = Stream
                .continually(d = new PolyDescriptor(data.next))
                .map(_ => if (List(0xFF,0xFE,0xFD).contains(d.value)) None else Some(nextPoly()))
                .takeWhile(p => p.isDefined)
                .map(p => createColoredPoly(p.get))
                .toList
            ui.drawFrame(polys)
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