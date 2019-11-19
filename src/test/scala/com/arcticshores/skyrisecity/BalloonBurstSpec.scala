package com.arcticshores.skyrisecity

import com.arcticshores.skyrisecity.baloons._
import org.scalatest.{FunSpec, GivenWhenThen, Matchers}

import scala.util.Failure

class BalloonBurstSpec extends FunSpec with GivenWhenThen with Matchers {

  describe("Balloon Burst creation") {
    it("should only start with at least one balloon") {
      BalloonBurstGame.start(Nil) shouldBe a[Failure[Active]]
    }
  }

  describe("Balloon Burst with single balloon") {
    it("banking straight away should end game with score of 0") {
      Given("game with a single balloon")
      val game = BalloonBurstGame.start(Balloon(1) :: Nil).get

      When("asking to 'bank'")
      val bankedGame = game.bankCurrentBalloon()

      Then("game should end with score of 0")
      bankedGame shouldBe Finished(0)
    }
  }

  it("bursting only available balloon should end game with score of 0") {
    Given("game with a single balloon")
    val game = BalloonBurstGame.start(Balloon(1) :: Nil).get

    When("asking to 'inflate' for the first time")
    val (_, inflatedOnce) = game.inflateCurrentBalloon()

    And("inflating one more time")
    val (status, inflatedTwice) = inflatedOnce.asInstanceOf[Active].inflateCurrentBalloon()

    Then("it should burst")
    status shouldBe Burst

    And("game should end with score of 0")
    inflatedTwice shouldBe Finished(0)
  }

  it("banking in appropriate moment should end game with positive score") {
    Given("game with a single balloon")
    val game = BalloonBurstGame.start(Balloon(2) :: Nil).get

    When("asking to 'inflate' for the first time")
    val (_, inflatedOnce) = game.inflateCurrentBalloon()

    And("asking to 'bank'")
    val banked = inflatedOnce.asInstanceOf[Active].bankCurrentBalloon()

    And("game should end with score of 1")
    banked shouldBe Finished(1)
  }

  describe("Balloon Burst with more than one balloon") {
    it("should properly calculate result") {
      Given("a game with multiple balloons")
      val game = BalloonBurstGame.start(Balloon(2) :: Balloon(4) :: Balloon(1) :: Nil).get

      When("bursting first balloon")
      val (_, firstBalloonOnce) = game.asInstanceOf[Active].inflateCurrentBalloon()
      val (_, firstBalloonTwice) = firstBalloonOnce.asInstanceOf[Active].inflateCurrentBalloon()
      val (_, firstBalloonThirdTime) = firstBalloonTwice.asInstanceOf[Active].inflateCurrentBalloon()

      And("inflating second balloon a little bit")
      val (_, secondBalloonOnce) = firstBalloonThirdTime.asInstanceOf[Active].inflateCurrentBalloon()
      val secondBalloonBanked = secondBalloonOnce.asInstanceOf[Active].bankCurrentBalloon()

      And("inflating third balloon a little bit")
      val (_, thirdBalloonOnce) = secondBalloonBanked.asInstanceOf[Active].inflateCurrentBalloon()
      val endResult = thirdBalloonOnce.asInstanceOf[Active].bankCurrentBalloon()

      Then("game should end with score of 2")
      endResult shouldBe Finished(2)
    }
  }
}
