package com.arcticshores.skyrisecity

import com.arcticshores.skyrisecity.baloons._

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

object ConsoleBalloonBurst {

  def main(args: Array[String]): Unit = {
    val maybeGame = for {
      balloons <- readBalloons()
      game <- BalloonBurstGame.start(balloons)
    } yield game

    maybeGame match {
      case Success(game) => interactWith(game)
      case Failure(e) => throw e
    }
  }

  @scala.annotation.tailrec
  private def interactWith(currentRound: BalloonBurstGame): Unit =
    currentRound match {
      case Finished(score) => displayScore(score)
      case inProgress: Active =>
        val nextCommand = readCommand()
        nextCommand match {
          case "INFLATE" =>
            val (status, nextRound) = inProgress.inflateCurrentBalloon()
            displayActionStatus(status)
            interactWith(nextRound)
          case "BANK" =>
            interactWith(inProgress.bankCurrentBalloon())
          case _ =>
            onUnrecognizedCommand()
        }
    }

  private def readBalloons(): Try[List[Balloon]] = Try {
    StdIn.readLine()
      .split("\\s+")
      .map(_.toInt)
      .map(Balloon(_))
      .toList
  }

  private def readCommand(): String =
    StdIn.readLine()
      .trim
      .toUpperCase

  private def displayActionStatus(status: InflationStatus): Unit =
    status match {
      case Burst => println("BURST")
      case _ =>
    }

  private def displayScore(score: Int): Unit =
    println(s"SCORE: $score")

  private def onUnrecognizedCommand(): Unit =
    println("Unrecognized command, possible values: [INFLATE|BANK]")
}
