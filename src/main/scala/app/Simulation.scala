package app

import math._

/**
  * Created by Jason on 9/22/2016.
  */
object Simulation {

  class CoordinatePair(val x:Double, val y:Double)

  class Point(override val x:Double, override val y:Double) extends CoordinatePair(x, y) {

    def distance(other:Point):Double = {
      sqrt(pow(x - other.x, 2)+pow(y - other.y, 2))
    }
    override def toString: String = "Point(" + x + ", " + y + ")"

    def vectorTo(other:Point):MathVector = MathVector(this, other)
  }
  object Point {
    def apply(x:Double, y:Double) = new Point(x, y)
  }


  class MathVector(override val x:Double, override val y:Double) extends CoordinatePair(x, y) {

    lazy val magnitude:Double = sqrt(pow(x,2)+pow(y,2))

    def normalized:MathVector = MathVector(x/magnitude, y/magnitude)

    override def toString: String = "Vector(" + x + ", " + y + ")"
  }

  object MathVector {
    def apply(x:Double, y:Double) = new MathVector(x,y)

    def apply(p1:Point, p2:Point) = new MathVector(p2.x - p1.x, p2.y - p1.y)
  }






  //case class Point(x:Int, y:Int)
  //case class DoublePoint(x:Double, y:Double)

  val checkpoints = scala.Vector(Point(500, 500))

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



  def calculateVelocity(position:Point, checkpoint:Point, currentVelocity:MathVector, angle:Double, thrust:Int):MathVector = {

    val checkpointVector = position.vectorTo(checkpoint)
    val correctionAngle:Double = {
      val angle = atan(checkpointVector.y/checkpointVector.x)*180/Pi
      if (checkpointVector.x > 0)
        angle
      else if (checkpointVector.y > 0)
        180 + angle
      else
        -180 + angle
    }

    val correctedAngle = correctionAngle - angle

    //debug
    Console.err.println("Checkpoint vector: " + checkpointVector)
    Console.err.println("Correction angle: " + correctionAngle)
    Console.err.println("angle: " + angle)
    Console.err.println("Corrected Angle: " + correctedAngle)


    MathVector(currentVelocity.x + thrust *cos(correctedAngle*Pi/180), currentVelocity.y + thrust*sin(correctedAngle*Pi/180))
  }

  def nextVelocity(velocity:MathVector):MathVector = {
    MathVector(velocity.x*.85, velocity.y*.85)
  }

  def nextPosition(position:Point, velocity:MathVector):Point = {
    val nextX = position.x + velocity.x
    val nextY = position.y + velocity.y
    Point(nextX, nextY)
  }



  // get the next angle for a pod
  def nextAngle(currentAngle: Int, position:Point, checkpoint:Point, target:Point ):Double = {

    val checkpointVector = MathVector(position, checkpoint)
    val targetVector = MathVector(position, target)
    val checkpointTargetVector = MathVector(checkpoint, target)
    
    val normalCheckpointVector = checkpointVector.normalized
    val normalTargetVector = targetVector.normalized

    val dotProduct = normalCheckpointVector.x * normalTargetVector.x + normalCheckpointVector.y * normalTargetVector.y
    val crossProduct = normalCheckpointVector.y * normalTargetVector.x - normalCheckpointVector.x * normalTargetVector.y

    val angleMagnitude = acos(dotProduct)*180/Pi
    val angleDirection = asin(crossProduct)*180/Pi

    val targetAngle = if (angleDirection > 0) angleMagnitude else angleMagnitude * -1

    //debug
    Console.err.println("Checkpoint unit vector: " + normalCheckpointVector)
    Console.err.println("Target unit vector: " + normalTargetVector)
    Console.err.println("Dot product: " + dotProduct)
    Console.err.println("Cross product: " + crossProduct)
    Console.err.println("Angle needed to complete turn: " + targetAngle)
    Console.err.println("pos-checkpoint magnitude: " + checkpointVector.magnitude)
    Console.err.println("target-checkpoint magnitude: " + checkpointTargetVector.magnitude)

    currentAngle + max(min(targetAngle - currentAngle, 18),-18)
  }

}
