package simulator.strategy

import scala.util.Random
import simulator._

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