package app

import math._

/**
  * Created by Jason on 9/22/2016.
  */
object Simulation {

  class Point(val x:Double, val y:Double)  {
    def distance(other:Point):Double = {
      sqrt(pow(x - other.x, 2)+pow(y - other.y, 2))
    }

    lazy val magnitude:Double = sqrt(pow(x,2)+pow(y,2))
    def normalized:Point = Point(x/magnitude, y/magnitude)
    override def toString: String = "Point(" + x + ", " + y + ")"
  }
  object Point {
    def apply(x:Double, y:Double) = new Point(x, y)
  }





  //case class Point(x:Int, y:Int)
  //case class DoublePoint(x:Double, y:Double)

  val checkpoints = Vector(Point(500, 500))

  var currentCheckpoint:Map[Player, Point] = Map.empty[Player, Point]


  case class Player(position:Point, velocity:Point, angle:Int)


  var player = Player(Point(200, 200), Point(0,0), 0)


  //assign player starting position and checkpoint
  def init():String = {
      //set up checkpoint list

      //set up player
      player = Player(Point(200, 200), Point(0,0), 0)

      //assign player to first checkpoint

      player.position.x + " " + player.position.y +
      " " + currentCheckpoint(player).x + " " + currentCheckpoint(player).y +
      -1 + " " + 180
  }


  def updated(input:String): String = {
    return ""
  }



  // get the next angle for a pod
  def nextAngle(currentAngle: Int, position:Point, target:Point, checkpoint:Point):Int = {

    val checkpointVector = Point(checkpoint.x - position.x, checkpoint.y - position.y)
    val targetVector = Point(target.x - position.x, target.y - position.y)
    val checkpointTargetVector = Point(checkpoint.x - target.x, checkpoint.y - target.y)
    
    val normalCheckpointVector = checkpointVector.normalized
    val normalTargetVector = targetVector.normalized

    val crossProduct = normalCheckpointVector.y * normalTargetVector.x - normalCheckpointVector.x * normalTargetVector.y
    val targetAngle = {
      val crossProdAngle = asin(crossProduct)*180/Pi
      // if checkpoint-target vector is longer than position-checkpoint vector, the triangle formed is oblique
      if (normalCheckpointVector.magnitude > checkpointTargetVector.magnitude)
        crossProdAngle
      else if (crossProdAngle > 0)  //true angle is > 90, so use form of 180-angle to get complement
        180 - crossProdAngle
      else
        -180 - crossProdAngle
    }

    currentAngle + max(min(targetAngle - currentAngle, 18),-18).toInt // angle is truncated in the game
  }

}
