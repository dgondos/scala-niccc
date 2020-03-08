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

    val bis = new BufferedInputStream(new FileInputStream("scene1.bin"))
    val bin: Array[Byte] = Stream.continually(bis.read).takeWhile(-1 !=).map(_.toByte).toArray

    def startDraw : Unit = {
        var palette = Array.fill(16)(Color.black)
        for (b_i <- 0 until 9) {
            val block = bin.slice(b_i * 64 * 1024, (b_i + 1) * 64 * 1024)
            val it = block.iterator
            def readByte : Int = {
                it.next & 0xFF // stupid java
            }
            var endOfBlock = false

            while (!endOfBlock) { // variable frames per block
                val flag = new FrameFlag(readByte)
                if (flag.isPalette) {
                    def mkWord16eb (l: Int, r: Int) : Int = {
                        ((l << 8) | r)
                    }
                    val bitmask = mkWord16eb(readByte, readByte)
                    for (i <- 0 to 15) {
                        val set = (bitmask & (1L << i)) != 0
                        if (set) {
                            palette.update(15 - i, decodeAtariSTColor(mkWord16eb(readByte, readByte)))
                        }
                    }
                }

                val indexedVerts = if (flag.isIndexed) (for (i <- 0 until readByte) yield i).map(_ => new Point(readByte, readByte)).toArray else Array[Point]()
                var polys = List[(List[Point], Color)]()

                breakable {
                    while (true) { // read until encountering special descriptor flag
                        val d = new PolyDescriptor(readByte)
                        d.value match {
                            case 0xFF => {
                                ui.updatePolys(polys)
                                ui.repaint
                                Thread.sleep(33)
                                break
                            }
                            case 0xFE => {
                                endOfBlock = true
                                break
                            }
                            case 0xFD => {
                                endOfBlock = true
                                break
                            }
                            case _ => { // data
                                val verts = (for (i <- 0 until d.nbrVerts) yield i).map(_ => if (flag.isIndexed) indexedVerts(readByte) else new Point(readByte, readByte)).toList
                                polys = polys :+ (verts, palette(d.colorIdx))
                            }
                        }
                    }
                }
            }
        }
    }

}