import org.scalajs.dom
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.CanvasRenderingContext2D

import scala.collection.mutable.ArrayBuffer
import scala.util.Random
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}


case class CellInputs(
                       nutrientsAvailable: Boolean,
                       nutrientLevelHalfway: Boolean,
                       nutrientLevelQuarterway: Boolean
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

class RandomCellActionDecider(val seed: Long) extends CellActionDecider {
  val rng = new Random(seed)

  def decide(cellInputs: CellInputs): CellAction = rng.nextInt(5) match {
    case 0 => Tumble
    case 1 => MoveStraight
    case 2 => UptakeNutrient
    case 3 => Split
    case 4 => Idle
  }
}

@JSExportTopLevel("Main")
object Main extends Movement  {

  @JSExport
  def start(canvas: Canvas): Unit = {

    implicit val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    val canvasEdgeSize = 200
    val fps = 10
    val seed = 100L
    val uptakeQuantity = 20
    val moveCost = 2
    val tumbleCost = 2
    val uptakeNutrientsCost = 2
    val splitCost = 4
    val idleCost = 1
    val bacteriaMaxNutrients = 100
    val bacteriaInitialNutrients = 50

    val nutrientsSeeder: NutrientSeeder = new UniformNutrientSeeder(initialValue = 100)
    val nutrients = new Nutrients(canvasEdgeSize, NutrientOptions(darkestAtNumberOfParticles = 100, seed = seed))
    var bacteria = new RandomBacteriaSeeder(
      p = 0.005,
      seed = seed,
      canvasEdgeSize = canvasEdgeSize,
      initialNutrients = bacteriaInitialNutrients
    ).seed()

    val actionDecider: CellActionDecider = new RandomCellActionDecider(seed = seed)

    val halfMaxNutrients = bacteriaMaxNutrients / 2
    val quarterMaxNutrients = bacteriaMaxNutrients / 4

    ctx.canvas.width = canvasEdgeSize
    ctx.canvas.height = canvasEdgeSize

    def clear(): Unit = {
      ctx.fillStyle = "black"
      ctx.fillRect(0, 0, canvasEdgeSize, canvasEdgeSize)
    }

    def draw(): Unit = {
      nutrients.draw()

      bacteria.foreach {
        case Cell(Position(x,y), nutrientLevel, _) if nutrientLevel <= 0 =>
          ctx.fillStyle = "brown"
          ctx.fillRect(x, y, 1, 1)
        case Cell(Position(x,y), _, _) =>
          ctx.fillStyle = "red"
          ctx.fillRect(x, y, 1, 1)
      }
    }

    nutrientsSeeder.seed(nutrients)

    def render(): Unit = {
      clear()
      draw()
    }

    def compute(): Unit = {
      nutrients.diffuse()

      val newBacteria = new ArrayBuffer[Cell]()

      bacteria = bacteria
        .filter(_.nutrientLevel > 0)
        .map { bact =>

          val tileLevel = nutrients(bact.position)

          val inputs = CellInputs(
            nutrientsAvailable = tileLevel > 0,
            nutrientLevelHalfway = bact.nutrientLevel < halfMaxNutrients,
            nutrientLevelQuarterway = bact.nutrientLevel < quarterMaxNutrients
          )

          actionDecider.decide(inputs) match {
            case MoveStraight =>
              val newPosition = move(bact.position, bact.orientation, canvasEdgeSize)

              bact.copy(
                position = newPosition,
                nutrientLevel = bact.nutrientLevel - moveCost
              )

            case Tumble =>
              val newOrientation = bact.orientation match {
                case Up =>    Right
                case Down =>  Left
                case Left =>  Up
                case Right => Down
              }
              bact.copy(
                orientation = newOrientation,
                nutrientLevel = bact.nutrientLevel - tumbleCost
              )

            case UptakeNutrient =>

              if(tileLevel > 0) {
                val effectiveUptakeQuantity = Math.min(uptakeQuantity, bacteriaMaxNutrients - bact.nutrientLevel)
                val newTileLevel = Math.max(0, tileLevel - effectiveUptakeQuantity)
                val absorbedNutrients = tileLevel - newTileLevel
                nutrients.update(bact.position, newTileLevel)
                bact.copy(nutrientLevel = absorbedNutrients - uptakeNutrientsCost)
              }
              else
                bact

            case Split =>
              val selfNutrients: Int = bact.nutrientLevel / 2
              val daughterNutrients = bact.nutrientLevel - selfNutrients

              newBacteria.append(
                Cell(
                  position = move(bact.position, bact.orientation, canvasEdgeSize),
                  orientation = bact.orientation,
                  nutrientLevel = daughterNutrients
                )
              )

              bact.copy(nutrientLevel = selfNutrients - splitCost)

            case _ =>
              bact.copy(
                nutrientLevel = bact.nutrientLevel - idleCost
              )
          }
        }

      bacteria.append(newBacteria:_*)

    }

    dom.window.setInterval(() => {
      compute()
      render()
    }, 1000 / fps)
  }

}