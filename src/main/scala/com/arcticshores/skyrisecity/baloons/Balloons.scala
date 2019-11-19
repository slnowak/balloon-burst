package com.arcticshores.skyrisecity.baloons

import scala.util.{Success, Try}

case class Balloon(burstLimit: Int, inflateAttempts: Int = 0) {
  private[baloons] def tryToInflate(): Try[Balloon] =
    Success(copy(inflateAttempts = inflateAttempts + 1))
      .filter(b => b.inflateAttempts <= b.burstLimit)
}

sealed trait InflationStatus

case object Inflated extends InflationStatus

case object Burst extends InflationStatus

sealed trait BalloonBurstGame

object BalloonBurstGame {
  def start(balloons: List[Balloon]): Try[Active] = for {
    initialBalloon <- Try(balloons.head)
  } yield new Active(initialBalloon, balloons.tail, 0)
}

class Active private[baloons](currentBalloon: Balloon, balloonsLeft: List[Balloon], currentScore: Int)
  extends BalloonBurstGame {

  def inflateCurrentBalloon(): (InflationStatus, BalloonBurstGame) =
    currentBalloon
      .tryToInflate()
      .fold(
        _ => Burst -> takeNextBalloon(bonus = 0),
        inflated => Inflated -> new Active(inflated, balloonsLeft, currentScore)
      )

  def bankCurrentBalloon(): BalloonBurstGame =
    takeNextBalloon(bonus = currentBalloon.inflateAttempts)

  private def takeNextBalloon(bonus: Int): BalloonBurstGame =
    balloonsLeft match {
      case nextBalloon :: left => new Active(nextBalloon, left, currentScore + bonus)
      case Nil => Finished(currentScore + bonus)
    }
}

case class Finished(score: Int) extends BalloonBurstGame