package simulator.strategy

import scala.util.Random
import simulator._

class ConservativeCellActionDecider(val seed: Long) extends CellActionDecider {
  val rng = new Random(seed)

  def decide(cellInputs: CellInputs): CellAction = {
    if (cellInputs.nutrientsAvailable) {
      if (cellInputs.nutrientLevelAtLeastHalfway) Split
      else if (!cellInputs.nutrientLevelAtLeastQuarterway) UptakeNutrient
      else Idle
    }
    else {
      rng.nextBoolean() match {
        case true => Tumble
        case false => MoveStraight
      }
    }
  }
}
