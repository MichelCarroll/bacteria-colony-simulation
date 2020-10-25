
import org.scalajs.dom.raw.CanvasRenderingContext2D

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

package object simulator {


  case class CellInputs(
                         nutrientsAvailable: Boolean,
                         nutrientLevelAtLeastHalfway: Boolean,
                         nutrientLevelAtLeastQuarterway: Boolean
                       )
  sealed trait CellAction
  case object Tumble extends CellAction
  case object MoveStraight extends CellAction
  case object UptakeNutrient extends CellAction
  case object Split extends CellAction
  case object Idle extends CellAction

  sealed trait Direction
  case object Up extends Direction
  case object Down extends Direction
  case object Left extends Direction
  case object Right extends Direction

  case class Position(x: Int, y: Int)
  case class Cell(position: Position, nutrientLevel: Int, orientation: Direction)

  case class NutrientOptions(darkestAtNumberOfParticles: Int, seed: Long)

  trait Movement {

    def clamp(canvasEdgeSize: Int, position: Int): Int =
      if(position >= 0) position % canvasEdgeSize else canvasEdgeSize + position

    def move(position: Position, orientation: Direction, canvasEdgeSize: Int): Position = {
      val Position(x,y) = position
      orientation match {
        case Up =>    Position(clamp(canvasEdgeSize, x),   clamp(canvasEdgeSize, y-1))
        case Down =>  Position(clamp(canvasEdgeSize, x),   clamp(canvasEdgeSize, y+1))
        case Left =>  Position(clamp(canvasEdgeSize, x-1), clamp(canvasEdgeSize, y))
        case Right => Position(clamp(canvasEdgeSize, x+1), clamp(canvasEdgeSize, y))
      }
    }
  }

  class Nutrients(val canvasEdgeSize: Int, val options: NutrientOptions)
                 (implicit val ctx: CanvasRenderingContext2D) extends  Movement {
    private var data = ctx.createImageData(canvasEdgeSize, canvasEdgeSize)
    private var doubleBuffer = ctx.createImageData(canvasEdgeSize, canvasEdgeSize)
    private val rng = new Random(options.seed)

    private def indexAt(x: Int, y: Int): Int =
      (y * canvasEdgeSize + x) * 4

    for {
      x <- 0 until canvasEdgeSize
      y <- 0 until canvasEdgeSize
      pixelIndex = indexAt(x, y)
    } {
      data.data.update(pixelIndex, 0)
      doubleBuffer.data.update(pixelIndex, 0)
      data.data.update(pixelIndex+1, 0)
      doubleBuffer.data.update(pixelIndex+1, 0)
      data.data.update(pixelIndex+2, 0)
      doubleBuffer.data.update(pixelIndex+2, 0)
      data.data.update(pixelIndex+3, 0)
      doubleBuffer.data.update(pixelIndex+3, 0)
    }

    def apply(position: Position): Int = data.data(indexAt(position.x, position.y)+3)

    def update(position: Position, particleNumber: Int): Unit = {
      data.data.update(indexAt(position.x, position.y)+3, Math.min(100, (particleNumber.toDouble / options.darkestAtNumberOfParticles * 100).toInt))
    }

    def draw(): Unit = {
      ctx.putImageData(data, 0, 0)
    }

    def diffuse(): Unit = {

      def add(x: Int, y: Int, value: Int): Unit = {
        val wrappedX = clamp(canvasEdgeSize, x)
        val wrappedY = clamp(canvasEdgeSize, y)
        val idx = indexAt(wrappedX, wrappedY)+3
        doubleBuffer.data.update(idx, doubleBuffer.data(idx) + value)
      }

      for {
        x <- 0 until canvasEdgeSize
        y <- 0 until canvasEdgeSize
        originIdx = indexAt(x, y)+3
        n = data.data(originIdx)
        if n > 0
      } {
        data.data.update(originIdx, 0)
        val partitionEdges = (1 to 4).map(_ => rng.nextInt(n)).sorted.toArray
        add(x, y, partitionEdges(0))
        add(x+1, y, partitionEdges(1) - partitionEdges(0))
        add(x, y+1, partitionEdges(2) - partitionEdges(1))
        add(x, y-1, partitionEdges(3) - partitionEdges(2))
        add(x-1, y, n - partitionEdges(3))
      }

      val tmp = data
      data = doubleBuffer
      doubleBuffer = tmp
    }

  }


  trait NutrientSeeder {
    def seed(nutrients: Nutrients): Unit
  }

  class UniformNutrientSeeder(initialValue: Int) extends NutrientSeeder {
    def seed(nutrients: Nutrients): Unit = {
      for {
        x <- 0 until nutrients.canvasEdgeSize
        y <- 0 until nutrients.canvasEdgeSize
        position = Position(x,y)
      } {
        nutrients.update(position,initialValue)
      }
    }
  }

  class RandomBacteriaSeeder(p: Double, seed: Long, canvasEdgeSize: Int, initialNutrients: Int) {
    private val rng = new Random(seed)

    def seed(): ArrayBuffer[Cell] = {
      val bacteria = new ArrayBuffer[Cell]()
      for {
        x <- 0 until canvasEdgeSize
        y <- 0 until canvasEdgeSize
      } {
        if(rng.nextDouble() < p) {
          bacteria.append(Cell(Position(x,y), nutrientLevel = initialNutrients, orientation = Up))
        }
      }
      bacteria
    }
  }


  trait CellActionDecider {
    def decide(cellInputs: CellInputs): CellAction
  }

}
