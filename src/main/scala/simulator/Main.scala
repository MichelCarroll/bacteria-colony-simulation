package simulator

import org.scalajs.dom
import org.scalajs.dom.html.Canvas

import scala.collection.mutable.ArrayBuffer
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import simulator._
import simulator.strategy.ConservativeCellActionDecider

@JSExportTopLevel("Main")
object Main extends Movement {

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

    //    val actionDecider: CellActionDecider = new RandomCellActionDecider(seed = seed)
    val actionDecider: CellActionDecider = new ConservativeCellActionDecider(seed = seed)


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
        case Cell(Position(x, y), nutrientLevel, _) if nutrientLevel <= 0 =>
          ctx.fillStyle = "brown"
          ctx.fillRect(x, y, 1, 1)
        case Cell(Position(x, y), _, _) =>
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
            nutrientLevelAtLeastHalfway = bact.nutrientLevel > halfMaxNutrients,
            nutrientLevelAtLeastQuarterway = bact.nutrientLevel > quarterMaxNutrients
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
                case Up => Right
                case Down => Left
                case Left => Up
                case Right => Down
              }
              bact.copy(
                orientation = newOrientation,
                nutrientLevel = bact.nutrientLevel - tumbleCost
              )

            case UptakeNutrient =>

              if (tileLevel > 0) {
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

      bacteria.append(newBacteria: _*)

    }

    dom.window.setInterval(() => {
      compute()
      render()
    }, 1000 / fps)
  }

}
